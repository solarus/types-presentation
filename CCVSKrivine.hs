{-# OPTIONS -fwarn-incomplete-patterns #-}

module CCVSKrivine where

import Syntax
import Parser

eval :: Term -> State
eval t = reduceFull (Clos (t, Empty), [])

reduceFull :: State -> State
reduceFull s = reduce s reduceFull

reduceStep :: State -> State
reduceStep s = reduce s id

reduce :: State -> (State -> State) -> State

-- lookup
reduce (Clos (Idx n, E (p, v)), s) f =
  if n == 0
  then f (v, s)
  else f (Clos (Idx (n - 1), p), s)
-- pop
reduce (Clos (Abs t, p), v : s) f =
  f (Clos (t, E (p, v)), s)
-- push
reduce (Clos (App t1 t2, p), s) f =
  f (Clos (t1, p), Clos (t2, p) : s)

-- abstractions are values (when stack is empty)
-- if environment is empty we are 'done',
-- otherwise we have non-evaluated args (OK, this is call-by-name, yo!)
reduce (Clos (Abs t, p), []) _ = (Clos (Abs t, p), [])

-- call-with-continuation rules
reduce (Clos (CC, _), v : s) f =
  f (v, Cont s : s)
reduce (Cont cs, v : s) f =
  f (v, cs)

-- NOTE: Halt if...

-- ...not all terms are closed
reduce (Clos (Idx _, Empty), _) f = error "term must be closed"
-- TODO cc with empty stack..?
reduce (Clos (CC, _), []) _ = error "cc with empty stack"
-- TODO what..?
reduce (Cont _,       []) _ = error "oh noes..?"

{-------------------}
{-- EXAMPLE TERMS --}
{-------------------}

-- (\x. x)
tid :: Term
tid = readTerm "/x.x"
-- tid = Abs (Idx 0)

-- (\x. x x)
delta :: Term
delta = readTerm "(/x.x x)"
--delta = Abs (App (Idx 0) (Idx 0))

-- simple evaluation
simpleex :: Term
simpleex = App delta tid

-- never-ending
omega :: Term
omega = App delta delta

true :: Term
true = readTerm "(/t./f.t)"
-- true = Abs (Abs (Idx 1))

-- call-by-name evaluation
cbnex :: Term
cbnex = App (App true tid) omega

triplex :: Term
triplex = readTerm "(/x.x x x)"
-- triplex = Abs (App (App (Idx 0) (Idx 0)) (Idx 0))

-- ever-growing
growex :: Term
growex = App triplex triplex

-- TEMP

tempt :: Term
tempt = readTerm "((/x./y.(x y)) ((/x.x) (/x.x)))"
-- tempt = App (Abs (Abs (App (Idx 1) (Idx 0)))) (App (Abs (Idx 0)) (Idx 0))

temps :: State
temps = (Clos (tempt,E (Empty,Clos (Idx 0,Empty))),[])

-- CC

noCCStr :: String
noCCStr = "(/x.x (/t./f.t) (/x.x)) (/t./f.f) (/t./f.t)"

withCCStr :: String
withCCStr = "cc " ++ noCCStr

noCC :: IO ()
noCC = do
  putStrLn noCCStr
  let t = eval (readTerm noCCStr)
  putStrLn (show t)

withCC :: IO ()
withCC = do
  putStrLn withCCStr
  let t = eval (readTerm withCCStr)
  putStrLn (show t)
