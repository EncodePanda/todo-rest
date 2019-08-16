module Todo where

import Polysemy
import Polysemy.Error
import KVS
import MonotonicSequence
import qualified Data.Map.Strict as M

type Key = Int

data TodoError = TodoNotAvailable Int

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

fetch :: ( Member (KVS Key Todo) r
       , Member (Error TodoError) r
       ) => Key -> Sem r Todo
fetch id = getKvs id >>= \case
          Just todo -> pure todo
          Nothing -> throw $ TodoNotAvailable id
