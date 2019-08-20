{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import Data.IORef
import Data.Function ((&))
import           KVS
import MonotonicSequence
import qualified           Network.Wai.Handler.Warp as W
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Servant.Server
import           Todo
import           TodoRest
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Options.Generic

initTodos :: M.Map Key Todo
initTodos = (M.singleton 1 (newTodo "push to repo"))

createApp :: IO Application
createApp = do
  idIORef <- newIORef 0
  kvsIORef <- newIORef initTodos
  return (serve api $ hoistServer api (\sem -> interpretServer sem idIORef kvsIORef) server)
  where
    interpretServer sem idIORef kvsIORef =  sem
                & runKvsOnMapState
                & runMonotonicSequenceOnState
                & runStateIORef @Int idIORef 
                & runStateIORef @(M.Map Key Todo) kvsIORef
                & runError @TodoError
                & runM
                & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (TodoNotAvailable id)) = Left err404 { errBody = "Todo does not exist" }
    handleErrors (Right value) = Right value

data Args = Args { port :: Int } deriving (Generic, Show)
instance ParseRecord Args

main :: IO ()
main = do
  Args(port) <- getRecord "TODO Server"
  app <- createApp
  W.run port app
