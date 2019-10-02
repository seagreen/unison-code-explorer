module Serve
  ( Config(..)
  , run
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Prelude
import Servant
import System.IO

import qualified Load
import qualified Network.Wai.Handler.Warp as Warp

-- * API

type ItemApi =

  -- These are meant for small libraries.
  -- We'll need to turn them off for the megaserver.

       "names" :> Get '[JSON] Load.Names
  :<|> "function-call-graph" :> Get '[JSON] Load.FunctionCallGraph

  -- Megaserver appropriate.

  :<|> "search" :> Capture "query-by-name" Text :> Get '[JSON] [()]

server :: Load.API -> Server ItemApi
server api =
       pure (Load.apiNames api)
  :<|> pure (Load.apiFcg api)
  :<|> (\txt -> liftIO $ (Load.apiSearch api) txt)

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
          (hPutStrLn stderr ("listening on port " <> show (configPort conf)))
          Warp.defaultSettings

mkApp :: Load.API -> IO Application
mkApp api =
  pure $ serve itemApi (server api)
