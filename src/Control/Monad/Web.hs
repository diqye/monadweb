{-# LANGUAGE TemplateHaskell,ConstraintKinds,GeneralizedNewtypeDeriving #-}
module Control.Monad.Web where
import Network.Wai.Handler.Warp(runEnv,run,Port)
import Data.Text(Text,splitOn)
import Control.Monad.State.Class
import Control.Monad.Trans(MonadTrans)
import Control.Monad.State.Strict
import Data.Default.Class
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Types as N
import qualified Network.HTTP.Types.URI as N
import qualified Network.Wai as N
import Control.Lens
import Control.Applicative((<|>),Alternative)
import Control.Monad(guard,forM_)
import Control.Monad.Except
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as L
import Data.String.Conversions(cs)
import qualified Data.Aeson as A
import Data.Aeson(
        FromJSON,
        ToJSON,
        encode,
        decode,
        Value,
        object,
        toJSON
    )


-- |  State required for web
data WebState = WebState {
    _request :: N.Request,
    _statePaths :: [Text], -- state paths
    _stateHeaders :: N.ResponseHeaders,
    _stateStatus :: N.Status
} deriving Show


data WebError = Error404 | Error500 Value deriving Show
instance Semigroup WebError where
    a@(Error500 _) <> _ = a
    _ <> b = b
instance Monoid WebError where
    mempty = Error404

type MonadWebState = MonadState WebState
type MonadWebError = MonadError WebError
type MonadWeb m = (MonadWebState m, Alternative m,MonadWebError m)

newtype WebT m a = WebT {
    unWebT :: StateT WebState (ExceptT WebError m) a
} deriving (Functor,Applicative,Monad,Alternative,MonadWebState, MonadWebError,MonadIO,MonadPlus)

instance MonadTrans WebT where
    lift = WebT . lift .lift

makeLenses ''WebState

-- | create Web from a request
fromRequest :: N.Request -> WebState
fromRequest req = def {
    _request = req ,
    _statePaths = N.pathInfo req
}
baseHeaders = [("Server","monadweb")]
instance Default WebState where
    def = WebState {
        _request = N.defaultRequest,
        _statePaths = [],
        _stateHeaders = baseHeaders,
        _stateStatus = N.status200
    }


(<--) :: (A.KeyValue kv, A.ToJSON v) => A.Key -> v -> kv
(<--) = (A..=)

createError :: ToJSON a => a -> WebError
createError message = Error500 $  object [
        "success" <-- False,
        "message" <-- toJSON message
    ] 

useRequest :: MonadWeb m => m N.Request
useRequest = use request


m2e :: Maybe b -> Either WebError b
m2e (Just b) = Right b
m2e _ = Left Error404

m2eWith :: Maybe b -> WebError -> Either WebError b
m2eWith (Just b) _ = Right b
m2eWith _ error = Left error

liftMaybe :: MonadWebError m => Maybe a -> m a
liftMaybe mayb = liftEither $ m2e mayb

liftMaybeWith :: MonadWebError m => WebError -> Maybe a -> m a
liftMaybeWith error mayb = liftEither $ m2eWith mayb error

-- | consume a path and return it
meeting :: MonadWeb m => m Text
meeting = do
    maybeH <- preuse $ statePaths . ix 0
    h <- liftMaybe maybeH 
    statePaths %= tail
    pure h
-- | home route
meetNull :: MonadWeb m => m ()
meetNull = do
    isNull <- use $ statePaths . to null
    guard isNull

-- | consume a path if it eauals the path param
meet :: MonadWeb m => Text -> m ()
meet path = do
    path' <- meeting
    guard $ path == path'

-- | consume mutiple path 
meetMutiple :: MonadWeb m => [Text] -> m ()
meetMutiple paths = forM_ paths meet

-- | meets "a/b/c" == meetList ["a","b","c"]
meets :: MonadWeb m => Text -> m ()
meets pathText = meetMutiple $ splitOn "/" pathText

-- | Like meet, but it's an exact match
matches :: MonadWeb m => Text -> m ()
matches pathText = meets pathText >> meetNull
-- | param
useQuery :: MonadWeb m => m N.Query
useQuery = do
  req <- useRequest
  let query = N.queryString req
  pure query

useQueryValue :: MonadWeb m => ByteString -> m ByteString
useQueryValue key = do
    query <- useQuery
    let error = createError $ (cs $ "The key \"" <> key <> "\" doesn't exist in querystring" :: Text)
    val <- liftMaybeWith error $ lookup key query
    pure $ maybe "" id val

useBody :: (MonadWeb m,MonadIO m) => m L.ByteString
useBody = do
    req <- useRequest
    liftIO $ N.strictRequestBody req

useBodyQuery :: (MonadWeb m,MonadIO m) => m N.Query
useBodyQuery = do
    body <- useBody
    pure $ N.parseQuery $ cs body

useBodyQueryValue :: (MonadWeb m,MonadIO m) => ByteString -> m ByteString
useBodyQueryValue key = do
    bodyQ <- useBodyQuery
    let error = createError $ (cs $ "The key \"" <> key <> "\" doesn't exist in querystring of body" :: Text)
    val <- liftMaybeWith error $ lookup key bodyQ
    pure $ maybe "" id val


useBodyJSON :: (MonadWeb m, MonadIO m,FromJSON a) => m a
useBodyJSON = do
    body <- useBody
    let a = A.eitherDecode body
    liftEither $ either (Left . createError) Right a

-- | methods
useMethod :: MonadWeb m => N.Method -> m ()
useMethod method = do
  req <- useRequest
  let method' = N.requestMethod req
  guard $ method' == method

useMethodGet :: MonadWeb m => m ()
useMethodGet = useMethod N.methodGet

useMethodPost :: MonadWeb m => m ()
useMethodPost = useMethod N.methodPost

useMethodPut :: MonadWeb m => m ()
useMethodPut = useMethod N.methodPut

useMethodDelete :: MonadWeb m => m ()
useMethodDelete = useMethod N.methodDelete

-- | headers
useRequestHeaders :: MonadWeb m => m N.RequestHeaders
useRequestHeaders = do
    request <- useRequest
    pure $ N.requestHeaders request

useRequestHeader :: MonadWeb m => N.HeaderName -> m ByteString
useRequestHeader key = do
    headers <- useRequestHeaders
    let val = lookup key headers
    liftMaybeWith (createError $ "The header name " <> show key <> " doesn't exist in this request") val

useHeader :: MonadWeb m => N.HeaderName -> ByteString -> m ()
useHeader name val = stateHeaders %= ((name,val):)

useHeaderJSON :: MonadWeb m => m ()
useHeaderJSON = useHeader "Content-Type" "application/json"

useHeaderHTML :: MonadWeb m => m ()
useHeaderHTML = useHeader  "Content-Type" "text/html; charset=utf-8"

-- | status
useStatus :: MonadWeb m => N.Status -> m ()
useStatus status = do
    stateStatus .= status

useStatus200 :: MonadWeb m => m ()
useStatus200 = useStatus N.status200
useStatus500 :: MonadWeb m => m ()
useStatus500 = useStatus N.status500
useStatus401 :: MonadWeb m => m ()
useStatus401 = useStatus N.status401

-- | resp
respLBS :: MonadWeb m => L.ByteString ->  m N.Response
respLBS bs = do
    status <- use stateStatus
    headers <- use stateHeaders
    pure $ N.responseLBS status headers bs


respJSON :: (MonadWeb m, ToJSON a) => a ->  m N.Response
respJSON a = do
    useHeaderJSON
    respLBS $ encode a

respStream :: MonadWeb m => N.StreamingBody -> m N.Response
respStream body = do
    status <- use stateStatus
    headers <- use stateHeaders
    pure $ N.responseStream status headers body

respFile :: MonadWeb m =>  FilePath -> Maybe N.FilePart -> m N.Response
respFile filepath part = do
    status <- use stateStatus
    headers <- use stateHeaders
    pure $ N.responseFile status headers filepath part

 -- | run
runWebT :: Monad m => WebT m a -> WebState -> m (Either WebError (a,WebState))
runWebT web webState = do
    a <- runExceptT $ runStateT (unWebT  web) webState
    pure a

type TransIO  m = m (Either WebError (N.Response, WebState)) -> IO (Either WebError (N.Response, WebState))
web2applicationT :: Monad m => WebT m N.Response -> TransIO m -> IO N.Application
web2applicationT web trans = pure $ \ req respond -> do
    let webState = fromRequest req
    let jsutDo (Left (Error404)) = respond $ N.responseLBS N.status404 baseHeaders ""
        jsutDo (Left (Error500 value)) = do 
            let headers = ("Content-Type","application/json"):baseHeaders
            respond $ N.responseLBS N.status500 headers $ encode value
        jsutDo (Right (response,_)) = respond response
    result <- trans $ runWebT web webState
    jsutDo result 

web2application :: WebT IO N.Response -> IO N.Application
web2application web = web2applicationT web id 

-- | Start a server on the port present in the PORT environment variable
--   Uses the Port 9999 when the variable is unset
runWebEnv ::  WebT IO N.Response -> IO ()
runWebEnv web = do
    app <- web2application web
    runEnv 9999 app 

runWeb :: Port -> WebT IO N.Response -> IO ()
runWeb port web = do
    app <- web2application web
    run port app 