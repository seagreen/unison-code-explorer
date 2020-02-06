module UCE.Serve
  ( Config(..)
  , run
  ) where

import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import UCE.Prelude

import qualified Network.Wai.Handler.Warp as Warp
import qualified UCE.Load as Load

-- * API

type ItemApi =
       "names" :> Get '[JSON] Load.Names
  :<|> "function-call-graph" :> Get '[JSON] Load.FunctionCallGraph

server :: Load.API -> Server ItemApi
server api =
       pure (Load.apiNames api)
  :<|> pure (Load.apiFcg api)

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
  api <- Load.load
  Warp.runSettings settings . simpleCors =<< mkApp api
  where
    settings :: Warp.Settings
    settings =
      Warp.setPort (configPort conf) $
        Warp.setBeforeMainLoop
          (logLn ("listening on port " <> show (configPort conf)))
          Warp.defaultSettings

mkApp :: Load.API -> IO Application
mkApp api =
  pure $ serve itemApi (server api)
