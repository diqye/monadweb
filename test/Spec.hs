{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import qualified Network.Wai as N
import Control.Monad.WebAll
import Data.Functor.Identity
import Control.Lens
import Data.String.Conversions(cs)
import Control.Concurrent.Async
import System.Process
import Data.List
import Data.Foldable(msum)
import Servant
import Data.Proxy
import qualified Data.Aeson as A


-- | 测试数据


testreq = N.defaultRequest  { N.pathInfo = ["a","b","c"] }
testweb = fromRequest testreq

getA :: WebT Identity a -> Maybe a
getA webt =  result where
    result = eitherA ^? _Right . _1
    eitherA = runIdentity $ runWebT webt testweb

-- | props

prop_meeting :: Bool
prop_meeting = getA meeting == Just "a"

prop_meeting_error :: Bool
prop_meeting_error = getA meeting /= Just "b"

prop_meet = getA (meet "a" ) == Just ()

prop_meetMutiple = getA (meetMutiple ["a","b","c"] ) == Just ()

prop_meets = getA (meets "a/b" ) == Just ()

prop_matches = getA (matches "a/b/c" ) == Just ()
prop_matches_error = getA (matches "a/b" ) == Nothing

prop_meets_error = getA (meets "a/b/c/d" ) == Nothing

prop_nullPath = r where
    r = getA t == Just ()
    t = do
        meets "a/b/c"
        meetNull

return []
runTests :: IO Bool
runTests = $quickCheckAll

ohmygod = do
    a <- useQueryValue "a"
    respLBS $ cs a
ohmyheader = do
     a <- useRequestHeader hAuthorization
     respLBS $ cs a
port = 8765
runServer = async $ runWeb port $ msum [
        meet "ok" >> respLBS "ok",
        meets "a/b/json" >> respJSON (1,2,3),
        meets "a/b" >> respJSON (4,6,6),
        matches "oh/my/god" >> useMethodGet >> ohmygod,
        matches "oh/my/header" >> ohmyheader,
        meet "servant" >> respApp app1
    ]




reqUrl rest = "http://localhost:" <> show port <> rest
reqContent url = do
    output <- readProcess "curl" ["-s",url] ""
    pure output

mycheck label action = do
    putStrLn $ "=== " <> label <> " ==="
    prop <- action
    quickCheck prop

io_prop_ok = do
    c <- reqContent $ reqUrl "/ok"
    pure $ c == "ok"
io_prop_json = do
    r <- reqContent $ reqUrl "/a/b/json"
    pure $ r == "[1,2,3]"
io_prop_json2 = do
    r <- reqContent $ reqUrl "/a/b"
    pure $ r == "[4,6,6]"
io_prop_query_error = do
    r <- reqContent $ reqUrl "/oh/my/god"
    pure $ ":false"  `isInfixOf` r

io_prop_query = do
    r <- reqContent $ reqUrl "/oh/my/god?a=myaa"
    pure $ r == "myaa"

io_prop_header = do
    r <- readProcess "curl" ["-s", "-H", "Authorization: my authorization",reqUrl "/oh/my/header"] ""
    pure $ r == "my authorization"

io_prop_servant = do
    r <- reqContent $ reqUrl "/servant/users"
    -- putStrLn r
    pure $ r == (cs $ A.encode users1)

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
main = do 
    putStrLn "========Basic prop==========="
    _ <- runTests
    putStrLn "========Server prop==========="
    asyncId <- runServer

    mycheck "io_prop_ok" io_prop_ok
    mycheck "io_prop_json" io_prop_json
    mycheck "io_prop_json2" io_prop_json2
    mycheck "io_prop_query_error" io_prop_query_error
    mycheck "io_prop_query" io_prop_query
    mycheck "io_prop_header" io_prop_header
    mycheck "io_prop_servant" io_prop_servant
    cancel asyncId
    -- wait asyncId