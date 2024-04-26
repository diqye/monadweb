
import Control.Monad.WebAll
import Data.Aeson
import Text.Show.Pretty (pPrint)


-- | auth all web
myAuthorization :: MonadWeb m => m Response
myAuthorization = do
    auth <- useRequestHeader hAuthorization <|> pure ""
    guard $ auth /= "my auth"
    -- set HTTP status  to response
    useStatus401
    -- set header to reponse
    useHeader "WWW-Authenticate" "Basic"
    respJSON $ object [
            "success" .= False,
            "message" .= "Authorization is required"
        ]

-- | /my/hello Get only 
myHello :: MonadWeb m => m Response
myHello =  do
    respLBS $ "my hello"

myjsonbody :: (MonadWeb m, MonadIO m) => m Response
myjsonbody = do
    a <- useBodyJSON
    respJSON $ (a::(String,Int,String))

main :: IO ()
main = runWebEnv $ msum [
        myAuthorization,
        meets "my/hello" >> useMethodGet >> myHello,
        meets "my/json/body" >> useMethodPost >> myjsonbody
    ]