module Syntax where

data Term = Idx Int | Nat Int | Abs Term | App Term Term | CC
     deriving Eq

data Value = Clos (Term, Environment) | Cont Stack
     deriving (Show,Eq)

-- for readability we have E (Value, Environment)
-- insteaf of              E (Environment, Value)
data Environment = Empty | E (Value, Environment)
     deriving (Show,Eq)

type Stack = [Value]

type State = (Value, Stack)

instance Show Term where
  show (Idx n)     = show n
  show (Nat n)     = "Nat: " ++ show n
  show (Abs t)     = "(λ." ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show CC          = "cc"
  