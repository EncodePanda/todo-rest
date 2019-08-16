{-# LANGUAGE TemplateHaskell #-}
module MonotonicSequence where

import Control.Monad ((>>))
import Polysemy
import Polysemy.State

data MonotonicSequence v m a where
  Next :: MonotonicSequence v m v

makeSem ''MonotonicSequence

runMonotonicSequenceOnState :: ( Member (State v) r
                            , Num v
                            ) => Sem ((MonotonicSequence v) ': r) a -> Sem r a
runMonotonicSequenceOnState = interpret $ \case
  Next -> modify (+1) >> get
