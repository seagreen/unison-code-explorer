module Serve
  ( Config(..)
  , run
  ) where

import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Prelude
import Servant
import System.IO

import qualified Load
import qualified Network.Wai.Handler.Warp as Warp

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
  , configPort :: Int
  } deriving (Show)

run :: Config -> IO ()
run conf = do
  (names, fcg) <- Load.load
  Warp.runSettings settings . simpleCors =<< mkApp names fcg
  where
    settings :: Warp.Settings
    settings =
      Warp.setPort (configPort conf) $
        Warp.setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " <> show (configPort conf)))
          Warp.defaultSettings

mkApp :: Load.Names -> Load.FunctionCallGraph -> IO Application
mkApp names fcg =
  pure $ serve itemApi (server names fcg)

server :: Load.Names -> Load.FunctionCallGraph -> Server ItemApi
server names fcg =
       pure names
  :<|> pure fcg
