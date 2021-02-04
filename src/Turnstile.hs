-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
module Turnstile where

import Control.Monad.State (State, state, put, get)

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

coinS :: State TurnstileState TurnstileOutput
coinS = state coin

pushS :: State TurnstileState TurnstileOutput
pushS = state push

pushS' :: State TurnstileState TurnstileOutput
pushS' = do
  s <- get
  put Locked
  case s of
    Locked -> return Tut
    Unlocked -> return Open

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS'
  a3 <- pushS'
  a4 <- coinS
  a5 <- pushS'
  return [a1, a2, a3, a4, a5]

testTurnstile :: State TurnstileState Bool
testTurnstile = do
  put Locked
  check1 <- pushS'
  put Unlocked
  check2 <- pushS'
  put Locked
  return (check1 == Tut && check2 == Open)