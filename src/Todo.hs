module Todo where

import Polysemy
import KVS
import MonotonicSequence
import qualified Data.Map.Strict as M

type Key = Int

data Todo = Todo { _title     :: String
                 , _completed :: Bool
                 }

newTodo :: String -> Todo
newTodo title  = Todo title False

add :: ( Member (KVS Key Todo) r
       , Member (MonotonicSequence Key) r) => Todo -> Sem r Key
add todo = do
  key <- next
  insertKvs key todo
  return key

list :: Member (KVS Key Todo) r => Sem r (M.Map Key Todo)
list = fmap M.fromList listAllKvs
