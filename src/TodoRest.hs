module TodoRest where

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
