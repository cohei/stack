{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Stack
  ( StackM
  , pop
  , push
  , runStack
  ) where

import Control.Monad.State (State, gets, put, modify, evalState)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Safe (headMay, tailMay)

-- | Fail all actions after pop empty stack
newtype StackM s a = StackM (MaybeT (State [s]) a)
  deriving (Functor, Applicative, Monad)

pop :: StackM s s
pop = StackM $ do
  i  <- MaybeT $ gets headMay
  is <- MaybeT $ gets tailMay
  put is
  return i

push :: s -> StackM s ()
push i = StackM $ modify (i:)

runStack :: StackM s a -> Maybe a
runStack (StackM m) = evalState (runMaybeT m) []
