module Serve where

import Network.Wai
import Network.Wai.Handler.Warp
import Prelude
import Servant
import System.IO
import Network.Wai.Middleware.Cors

import qualified Load

-- * API

type ItemApi =
       "names" :> Get '[JSON] Load.Names
  :<|> "function-call-graph" :> Get '[JSON] Load.FunctionCallGraph

itemApi :: Proxy ItemApi
itemApi =
  Proxy

-- * App

data Config = Config
  { configDumpJson :: Bool
  , configBranch :: Load.BranchName
  , configPort :: Int
  } deriving (Show)

run :: Config -> IO ()
run conf = do
  (names, fcg) <- Load.load (configBranch conf)
  runSettings settings . simpleCors =<< mkApp names fcg
  where
    settings :: Settings
    settings =
      setPort (configPort conf) $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " <> show (configPort conf)))
          defaultSettings

mkApp :: Load.Names -> Load.FunctionCallGraph -> IO Application
mkApp names fcg =
  pure $ serve itemApi (server names fcg)

server :: Load.Names -> Load.FunctionCallGraph -> Server ItemApi
server names fcg =
       pure names
  :<|> pure fcg
