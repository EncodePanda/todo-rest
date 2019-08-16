module Todo where

type Key = Int

data Todo = Todo { _title     :: String
                 , _completed :: Bool
                 }

newTodo :: String -> Todo
newTodo title  = Todo title False

