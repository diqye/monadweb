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
import Control.Monad.Web
main = runWebEnv 9999 $ respLBS "Hello world"
```
`runWebEnv` will start a server listening port 9999 returning only "Hello world". Why end with Env? Because we first look for the environment variable PORT as the port, and only use 9999 if we cannot find it.

## Routing with zero learning cost
```Haskell
iimport Data.Aeson
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
main = runWebEnv 9999 $ 
    myAuthorization <|>
    (meets "my/hello" >> useMethodGet >> myHello)
```

## Or using `msum`
```Haskell
myjsonbody :: (MonadWeb m, MonadIO m) => m Response
myjsonbody = do
    a <- useBodyJSON
    respJSON $ (a::(String,Int,String))

main :: IO ()
main = runWebEnv 9999 $ msum [
        myAuthorization,
        meets "my/hello" >> useMethodGet >> myHello,
        meets "my/json/body" >> useMethodPost >> myjsonbody
    ]
```
