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

data WebResponse = WebResponse N.Response | WebApplication N.Application

class HasWebState s where
    webStateLens :: Lens' s WebState
    fromWebState :: WebState -> s

class HasWebError e where 
    getWebError :: e -> WebError
    fromWebError :: WebError -> e

instance HasWebError WebError where
    getWebError e = e
    fromWebError webError = webError

instance HasWebState WebState where
    webStateLens = id
    fromWebState = id 

type MonadWebState s m = (MonadState s m, HasWebState s)
type MonadWebError e m = (MonadError e m, HasWebError e)
type MonadWeb s e m = (MonadWebState s m, Alternative m,MonadWebError e m)


newtype WebT s e m a = WebT {
    unWebT :: StateT s (ExceptT e m) a
} deriving (Functor,Applicative,Monad,Alternative,MonadState s, MonadError e,MonadIO,MonadPlus)

instance MonadTrans (WebT s e) where
    lift = WebT . lift .lift

makeLenses ''WebState

-- | create Web from a request
fromRequest :: HasWebState s => N.Request -> s
fromRequest req = fromWebState $ def {
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

createError ::  HasWebError e => ToJSON a => a -> e
createError message = fromWebError $ Error500 $  object [
        "success" <-- False,
        "message" <-- toJSON message
    ] 

useRequest :: MonadWebState s m => m N.Request
useRequest = use $ webStateLens . request

m2e :: (HasWebError e) => Maybe b -> Either e b
m2e (Just b) = Right b
m2e _ = Left $ fromWebError Error404

m2eWith :: (HasWebError e) => Maybe b -> WebError -> Either e b
m2eWith (Just b) _ = Right b
m2eWith _ error = Left $ fromWebError error

liftMaybe :: MonadWebError e m => Maybe a -> m a
liftMaybe mayb = liftEither $ m2e mayb

liftMaybeWith :: MonadWebError e m => WebError -> Maybe a -> m a
liftMaybeWith error mayb = liftEither $ m2eWith mayb error

-- | consume a path and return it
meeting :: MonadWeb s e m => m Text
meeting = do
    maybeH <- preuse $ webStateLens . statePaths . ix 0
    h <- liftMaybe maybeH 
    webStateLens . statePaths %= tail
    pure h
-- | home route
meetNull :: MonadWeb s e m => m ()
meetNull = do
    isNull <- use $ webStateLens . statePaths . to null
    guard isNull

-- | consume a path if it eauals the path param
meet :: MonadWeb  s e m => Text -> m ()
meet path = do
    path' <- meeting
    guard $ path == path'

-- | consume mutiple path 
meetMutiple :: MonadWeb s e m => [Text] -> m ()
meetMutiple paths = forM_ paths meet

-- | meets "a/b/c" == meetList ["a","b","c"]
meets :: MonadWeb s e m => Text -> m ()
meets pathText = meetMutiple $ splitOn "/" pathText

-- | Like meet, but it's an exact match
matches :: MonadWeb s e m => Text -> m ()
matches pathText = meets pathText >> meetNull
-- | param
useQuery :: MonadWeb s e m => m N.Query
useQuery = do
  req <- useRequest
  let query = N.queryString req
  pure query

useQueryValue :: MonadWeb s e m => ByteString -> m ByteString
useQueryValue key = do
    query <- useQuery
    let error = createError $ (cs $ "The key \"" <> key <> "\" doesn't exist in querystring" :: Text)
    val <- liftMaybeWith error $ lookup key query
    pure $ maybe "" id val

useBody :: (MonadWeb s e m,MonadIO m) => m L.ByteString
useBody = do
    req <- useRequest
    liftIO $ N.strictRequestBody req

useBodyQuery :: (MonadWeb s e m,MonadIO m) => m N.Query
useBodyQuery = do
    body <- useBody
    pure $ N.parseQuery $ cs body

useBodyQueryValue :: (MonadWeb s e m,MonadIO m) => ByteString -> m ByteString
useBodyQueryValue key = do
    bodyQ <- useBodyQuery
    let error = createError $ (cs $ "The key \"" <> key <> "\" doesn't exist in querystring of body" :: Text)
    val <- liftMaybeWith error $ lookup key bodyQ
    pure $ maybe "" id val


useBodyJSON :: (MonadWeb  s e m, MonadIO m,FromJSON a) => m a
useBodyJSON = do
    body <- useBody
    let a = A.eitherDecode body
    liftEither $ either (Left . createError) Right a

-- | methods
useMethod :: MonadWeb s e m => N.Method -> m ()
useMethod method = do
  req <- useRequest
  let method' = N.requestMethod req
  guard $ method' == method

useMethodGet :: MonadWeb s e m => m ()
useMethodGet = useMethod N.methodGet

useMethodPost :: MonadWeb s e m => m ()
useMethodPost = useMethod N.methodPost

useMethodPut :: MonadWeb s e m => m ()
useMethodPut = useMethod N.methodPut

useMethodDelete :: MonadWeb s e m => m ()
useMethodDelete = useMethod N.methodDelete

-- | headers
useRequestHeaders :: MonadWeb s e m => m N.RequestHeaders
useRequestHeaders = do
    request <- useRequest
    pure $ N.requestHeaders request

useRequestHeader :: MonadWeb s e m => N.HeaderName -> m ByteString
useRequestHeader key = do
    headers <- useRequestHeaders
    let val = lookup key headers
    liftMaybeWith (createError $ "The header name " <> show key <> " doesn't exist in this request") val

useHeader :: MonadWeb s e m => N.HeaderName -> ByteString -> m ()
useHeader name val = webStateLens . stateHeaders %= ((name,val):)

useHeaderJSON :: MonadWeb s e m => m ()
useHeaderJSON = useHeader "Content-Type" "application/json"

useHeaderHTML :: MonadWeb s e m => m ()
useHeaderHTML = useHeader  "Content-Type" "text/html; charset=utf-8"

-- | status
useStatus :: MonadWeb s e m => N.Status -> m ()
useStatus status = do
    webStateLens . stateStatus .= status

useStatus200 :: MonadWeb s e m => m ()
useStatus200 = useStatus N.status200
useStatus500 :: MonadWeb s e m => m ()
useStatus500 = useStatus N.status500
useStatus401 :: MonadWeb s e m => m ()
useStatus401 = useStatus N.status401

-- | resp
respLBS :: MonadWeb s e m => L.ByteString ->  m WebResponse
respLBS bs = do
    status <- use $ webStateLens . stateStatus
    headers <- use $ webStateLens . stateHeaders
    pure $ WebResponse $ N.responseLBS status headers bs


respJSON :: (MonadWeb s e m, ToJSON a) => a ->  m WebResponse
respJSON a = do
    useHeaderJSON
    respLBS $ encode a

respStream :: MonadWeb s e m => N.StreamingBody -> m WebResponse
respStream body = do
    status <- use $ webStateLens . stateStatus
    headers <- use $ webStateLens . stateHeaders
    pure $ WebResponse $ N.responseStream status headers body

respApp :: MonadWeb s e m => N.Application -> m WebResponse
respApp app = do
    paths <- use $ webStateLens . statePaths
    webStateLens . request  %= ( \ req -> req { N.pathInfo = paths }) 
    pure $ WebApplication app


respFile :: MonadWeb s e m =>  FilePath -> Maybe N.FilePart -> m WebResponse
respFile filepath part = do
    status <- use $ webStateLens . stateStatus
    headers <- use $ webStateLens . stateHeaders
    pure $ WebResponse $ N.responseFile status headers filepath part

 -- | run
runWebT :: (Monad m,HasWebState s,HasWebError e) => WebT s e m a -> s -> m (Either e (a,s))
runWebT web webState = do
    a <- runExceptT $ runStateT (unWebT  web) webState
    pure a

type TransIO s e m = m (Either e (WebResponse, s)) -> IO (Either e (WebResponse, s))
web2applicationT :: (Monad m,HasWebState s,HasWebError e) => WebT s e m WebResponse -> TransIO s e m -> IO N.Application
web2applicationT web trans = pure $ \ req respond -> do
    let webState = fromRequest req
    let jsutDo (Left e ) = case getWebError e of 
            Error404 -> respond $ N.responseLBS N.status404 baseHeaders ""
            (Error500 value) -> do
                let headers = ("Content-Type","application/json"):baseHeaders
                respond $ N.responseLBS N.status500 headers $ encode value
        jsutDo (Right (WebResponse response,_)) = respond response
        jsutDo (Right (WebApplication application,state)) =  do
            let req = state ^. webStateLens . request
            application req respond
    result <- trans $ runWebT web webState
    jsutDo result 

web2application :: (HasWebState s,HasWebError e) => WebT s e IO WebResponse -> IO N.Application
web2application web = web2applicationT web id 

-- | Start a server on the port present in the PORT environment variable
--   Uses the Port 9999 when the variable is unset
runWebEnv' :: (HasWebState s,HasWebError e) => WebT s e IO WebResponse -> IO ()
runWebEnv' web = do
    app <- web2application web
    runEnv 9999 app 

runWeb' :: (HasWebState s,HasWebError e) => Port -> WebT s e IO WebResponse -> IO ()
runWeb' port web = do
    app <- web2application web
    run port app 

-- | Start a server on the port present in the PORT environment variable
--   Uses the Port 9999 when the variable is unset
runWeb :: Port -> WebT WebState WebError IO WebResponse -> IO ()
runWeb = runWeb'

runWebEnv :: WebT WebState WebError IO WebResponse -> IO ()
runWebEnv = runWebEnv'