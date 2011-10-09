{-# OPTIONS -Wall #-}

module CCVSKrivine where

import Syntax
import Parser

import Control.Monad (void, when)

eval :: Bool -> Term -> IO State
eval debug t =
  let d (v,s) = do
        showValue v
        showStack s
        void getLine
      f df state lastState
        | state == lastState = return state
        | otherwise = do
          when df $ d lastState
          f df (reduce state) state
      start = (Clos (t, Empty), [])
  in f debug (reduce start) start

reduce :: State -> State

-- lookup
reduce (Clos (Idx n, E (v, p)), s) =
  if n == 0
    then (v, s)
    else (Clos (Idx (n - 1), p), s)
-- pop
reduce (Clos (Abs t, p), v : s) =
  (Clos (t, E (v, p)), s)
-- push
reduce (Clos (App t1 t2, p), s) =
  (Clos (t1, p), Clos (t2, p) : s)

-- abstractions are values (when stack is empty)
-- if environment is empty we are 'done',
-- otherwise we have non-evaluated args (OK, this is call-by-name, yo!)
reduce (Clos (Abs t, p), []) = (Clos (Abs t, p), [])

-- call-with-continuation rules
reduce (Clos (CC, _), v : s) =
  (v, Cont s : s)
reduce (Cont cs, v : _s) =
  (v, cs)
  
reduce (Clos (Nat n, p), s) = (Clos (Nat n, p), s)

-- NOTE: Halt if...

-- ...not all terms are closed
reduce (Clos (Idx _, Empty), _) = error "term must be closed"
-- TODO cc with empty stack..?
reduce (Clos (CC, _), []) = error "cc with empty stack"
-- TODO what..?
reduce (Cont _,       []) = error "oh noes..?"

{-------------------}
{-- EXAMPLE TERMS --}
{-------------------}

-- (\x. x)
tid :: Term
tid = readTerm "/x.x"

-- (\x. x x)
delta :: Term
delta = readTerm "(/x.x x)"

-- simple evaluation
simpleex :: Term
simpleex = App delta tid

-- never-ending
omega :: Term
omega = App delta delta

true :: Term
true = readTerm "(/t./f.t)"

-- call-by-name evaluation
cbnex :: Term
cbnex = App (App true tid) omega

triplex :: Term
triplex = readTerm "(/x.x x x)"

-- ever-growing
growex :: Term
growex = App triplex triplex

-- TEMP

tempt :: Term
tempt = readTerm "((/x./y.(x y)) ((/x.x) (/x.x)))"

temps :: State
temps = (Clos (tempt,E (Clos (Idx 0,Empty), Empty)),[])

-- CC

noCCStr :: String
noCCStr = "(/x.x (/t./f.t) (/x.x)) (/t./f.f) (/t./f.t)"

withCCStr :: String
withCCStr = "cc " ++ noCCStr

noCC :: Bool -> IO ()
noCC b = do
  putStrLn noCCStr
  t <- eval b (readTerm noCCStr)
  putStrLn (show t)

withCC :: Bool -> IO ()
withCC b = do
  putStrLn withCCStr
  t <- eval b (readTerm withCCStr)
  putStrLn (show t)

--------------------------------------------------
-- helpers

showValue :: Value -> IO ()
showValue (Clos (t,e)) = do
  putStrLn $ "Term  = " ++ show t
  showEnv e
showValue (Cont s) = do
  putStrLn "Continuation"
  showStack s
  putStrLn "====="

showStack :: Stack -> IO ()
showStack [] = putStrLn "Stack = []"
showStack (v:s) =
  let f []     = putStrLn " ]"
      f (v':s')  = do
        putStr $ "\n        , " ++ show v'
        f s'
  in do
    putStr "Stack = "
    putStr $ "[ " ++ show v
    f s

showEnv :: Environment -> IO ()
showEnv Empty = putStrLn "Env   = Empty"
showEnv (E (v,e)) =
  let f Empty      = putStrLn "\n        , Empty )"
      f (E (v',e')) = do
        putStr $ "\n        , " ++ show v'
        f e'
  in putStr ("Env   = ( " ++ show v) >> f e
  