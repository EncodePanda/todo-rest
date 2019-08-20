module TodoRest where

import Polysemy
import Polysemy.Error
import KVS
import MonotonicSequence
import Todo
import Servant
import Data.Proxy
import qualified Data.Map.Strict as M

type TodoAPI
  =    "todo" :> Get '[JSON] (M.Map Int Todo)
  :<|> "todo" :> Capture "id" Int :> Get '[JSON] Todo
  :<|> "todo" :> Capture "id" Int :> "toggle" :> Get '[JSON] Todo
  :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo

api :: Proxy TodoAPI
api = Proxy

server :: ( Member (KVS Key Todo) r
          , Member (Error TodoError) r
          , Member (MonotonicSequence Key) r
          ) => ServerT TodoAPI (Sem r)
server
  =    list
  :<|> fetch
  :<|> toggle
  :<|> addAndFetch

  where
    addAndFetch todo = (add todo) >>= fetch

