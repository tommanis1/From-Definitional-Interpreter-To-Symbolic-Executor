{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Sect02SemStd where

import Control.Monad.Except
import Control.Monad.Fail as Fail
import qualified Control.Monad.HT as HT
import Data.Tuple.HT
import Data.Maybe
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R


mmap :: Monad m => (a -> m b) -> [a] -> m [b]
mmap = HT.map

data Expr     =  Con String [Expr]
              |  Case Expr [(Patt, Expr)]
              |  Var String
              |  Lam String Expr
              |  App Expr Expr
              |  Let [(String, Expr)] Expr
              |  Letrec [(String, ValExpr)] Expr
              |  EEq Expr Expr
              deriving Show
              deriving Eq

data ValExpr  =  VCon String [ValExpr]
              |  VLam String Expr
              deriving Show
              deriving Eq

data Patt     =  PVar String
              |  PCon String [Patt]

              deriving Show
              deriving Eq

class (MonadEnv val m,
       MonadMatch val Cases m,
       TermVal val,
       FunVal val,
       FunApp val m,
       TermEq val m,
       MonadFail m) => EffVal m val where

resolve :: String -> Env v -> v
resolve x nv = fromJust (lookup x nv)
interp :: EffVal m val => Expr -> m val



interp (Con c es) = do
  vs <- mmap interp es
  return (con_v c vs)
interp (Case e bs) =
  let vbs = map (mapSnd interp) bs in do
  v <- interp e
  match v (Cases vbs)
interp (Var x) = do
  nv <- ask
  return (resolve x nv)
interp (Lam x e) = do
  nv <- ask
  return (clos_v x e nv)
interp (App e_1 e_2) = do
  f <- interp e_1
  a <- interp e_2
  app f a

-- interp (Let xes e) = do
--   nv <- mmap interpSnd xes
--   local (\ nv_0 -> nv ++ nv_0) (interp e)
--   where interpSnd (x, e) = do
--           v <- interp e; return (x, v)

interp (Letrec xves e) = do
  nv <- ask
  let  nv_b  = map (mapSnd (interpval nv_r)) xves
       nv_r  = nv_b ++ nv in
    local (\ _ -> nv_r) (interp e)
interp (EEq e_1 e_2) = do
  v_1 <- interp e_1
  v_2 <- interp e_2
  eq v_1 v_2


interpval ::  (TermVal val, FunVal val) =>
              Env val -> ValExpr -> val
interpval nv (VLam x e)   = clos_v x e nv
interpval nv (VCon x es)  =
  con_v x (map (interpval nv) es)




type Env val = [(String, val)]
class Monad m => MonadEnv val m where
  ask    :: m (Env val)
  local  :: (Env val -> Env val) -> m val -> m val

class Monad m => ClassicalMonadReader r m where
  ask_c    :: m r
  local_c  :: (r -> r) -> m a -> m a


class Monad m => MonadBranch cval rval fork m where
  branch :: cval -> fork m rval -> m rval

newtype IfThenElse m a = ITE (m a, m a)

instance  Monad m =>
          MonadBranch Bool rval IfThenElse m where
  branch True   (ITE (t, _)) = t
  branch False  (ITE (_, f)) = f 

class Monad m => MonadMatch val fork m where
  match :: val -> fork m val -> m val

newtype Cases m a = Cases [(Patt, m a)]

class TermVal val where
  con_v   :: String -> [val] -> val

class FunVal val where
  clos_v  :: String -> Expr -> Env val -> val

class FunApp val m where
  app     :: val -> val -> m val

class TermEq val m where
  eq      :: val -> val -> m val
data ConcreteValue  =  ConV String [ConcreteValue]
                    |  ClosV String Expr (Env ConcreteValue)
                   deriving Show
                   deriving Eq
type ConcreteMonad  =
  ReaderT (Env ConcreteValue) (Except String)

type ConcreteMonad' a =
  Env ConcreteValue -> Either String a

instance MonadMatch  ConcreteValue Cases
                     ConcreteMonad where
  match v (Cases ((p, m) : bs)) = case vmatch (v, p) of
      Just nv -> local (\ nv0 -> nv ++ nv0) m
      Nothing -> match v (Cases bs)
  match _ (Cases []) = throwError "Match failure"

vmatch ::  (ConcreteValue, Patt) ->
           Maybe (Env ConcreteValue)
vmatch (ConV s vs, PCon s' ps) =
  if (s == s' && length vs == length ps)
  then concat <$> mmap vmatch (zip vs ps)
  else Nothing
vmatch (v', PVar x)  = Just [(x, v')]
vmatch (_, _)        = Nothing

runSteps ::  Expr -> Env ConcreteValue ->
             Either String ConcreteValue
runSteps e nv = runExcept (runReaderT (interp e) nv)

instance TermVal ConcreteValue where
  con_v = ConV

instance FunVal ConcreteValue where
  clos_v = ClosV

instance {-# OVERLAPPING #-} Monad m => MonadFail (ExceptT String m) where
  fail s = throwError s

instance (Monad m, R.MonadReader (Env v) m) => MonadEnv v m where
  ask   = R.ask
  local = R.local

instance FunApp ConcreteValue ConcreteMonad where
  app (ClosV x e nv) v = local (\ _ -> (x, v) : nv) (interp e)
  app _ _ = throwError "Could not apply non-function"

instance TermEq ConcreteValue ConcreteMonad where
  eq v1 v2 | v1 == v2 = return (ConV "true" [])
  eq _  _  = return (ConV "false" [])

instance EffVal ConcreteMonad ConcreteValue where
