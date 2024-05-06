# monadweb

A web framework inspired by `mtl` package

## Install
stack.yaml
```yaml
extra-deps: 
  - git: https://github.com/diqye/monadweb.git
    commit: latest commit
```
package.yaml
```yaml
dependencies:
- monadweb
```

## Hello world
```Haskell
import Control.Monad.WebAll
main = runWebEnv $ respLBS "Hello world"
```
`runWebEnv` will start a server listening PORT(9999) returning only "Hello world". Why end with Env? Because we first look for the environment variable PORT as the port, and only use 9999 if we cannot find it.

## Routing 
```Haskell
import Control.Monad.WebAll
import Data.Aeson
import Text.Show.Pretty (pPrint)

-- | auth all web
myAuthorization :: MonadWeb m => m Response
myAuthorization = do
    auth <- useRequestHeader hAuthorization
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

main :: IO ()
main = runWebEnv $ 
    myAuthorization <|>
    (meets "my/hello" >> useMethodGet >> myHello)
```

### Or with `msum`
```Haskell
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
```

## Servant
`servant` and `servant-server`
```Haskell
type UserAPI1 = "users" :> Get '[JSON] [(String,String)]
users1 :: [(String,String)]
users1 = [
        ("1","2"),
        ("1","2"),
        ("3","2")
    ]
server1 :: Server UserAPI1
server1 = return users1
userAPI :: Proxy UserAPI1
userAPI = Proxy
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = runWebEnv $ msum [
        meet "servant" >> respApp app1
    ]
```
When visit `/servant/users`, it will respond with json `[["1","2"],["1","2"],["3","2"]]`
