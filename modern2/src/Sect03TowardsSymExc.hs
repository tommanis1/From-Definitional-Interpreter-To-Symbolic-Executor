
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

module Sect03TowardsSymExc where

import Control.Monad.Except
import Control.Monad.Fail
import Sect02SemStd
import Control.Monad.Reader (runReaderT)
import qualified Control.Monad.Reader as Reader

data Free c a  =  Stop a
               |  forall b. Step (c b) (b -> Free c a)
liftF :: c a -> Free c a
liftF c = Step c Stop

instance Functor (Free c) where
  fmap :: (a -> b) -> Free c a -> Free c b
  fmap g (Stop a)   = Stop (g a)
  fmap g (Step c k) = Step c (fmap g . k)

instance Applicative (Free c) where
  pure :: a -> Free c a
  pure = Stop

  (<*>) :: Free c (a -> b) -> Free c a -> Free c b
  Stop g     <*> f = fmap g f
  (Step c k) <*> f = Step c (\ x -> k x <*> f)

instance Monad (Free c) where
  return           = Stop

  Stop a    >>= k  = k a
  Step c f  >>= k  = Step c (\ x -> f x >>= k)

data Cmd val :: * -> * where
  Match  ::  val -> Cases (Free (Cmd val)) val ->
             Cmd val val
  Local  ::  (Env val -> Env val) -> Free (Cmd val) val ->
             Cmd val val
  Ask    ::  Cmd val (Env val)
  App_c  ::  val -> val -> Cmd val val
  Eq_c   ::  val -> val -> Cmd val val
  Fail   ::  String -> Cmd val a

instance MonadFail (Free (Cmd v)) where
  fail s = liftF (Fail s)

instance {-# OVERLAPPING #-} MonadEnv v (Free (Cmd v)) where
  ask = liftF Ask
  local f m = liftF (Local f m)

instance MonadMatch val Cases (Free (Cmd val)) where
  match v (Cases bs) = liftF (Match v (Cases bs))

instance FunApp val (Free (Cmd val)) where
  app f a = liftF (App_c f a)

instance TermEq val (Free (Cmd val)) where
  eq v_1 v_2 = liftF (Eq_c v_1 v_2)

instance (TermVal val, FunVal val) => EffVal (Free (Cmd val)) val where

comp ::  (TermVal val, FunVal val) =>
         Expr -> Free (Cmd val) val
comp =  interp

type Thread_c = Free (Cmd ConcreteValue)

step ::  Thread_c ConcreteValue ->
         ConcreteMonad (Thread_c ConcreteValue)
step (Stop x)                       = return (Stop x)
step (Step (Match _ (Cases [])) _)  =
  throwError "Pattern match failure"
step (Step (Match v (Cases ((p, m) : bs))) k) =
  case vmatch (v, p) of
    Just nv ->
      return (Step (Local (\ nv_0 -> nv ++ nv_0) m) k)
    Nothing -> step (Step (Match v (Cases bs)) k)

step (Step (Local f m) k) = do
  r <- Reader.local f (step m)
  return (case r of
            Stop x -> k x
            _      -> Step (Local f r) k)
step (Step Ask k) = do
  nv <- Reader.ask
  return (k nv)
step (Step (App_c (ClosV x e nv) a) k) = do
  return (Step (Local  (\ _ -> (x, a) : nv)
                       (interp e)) k)
step (Step (App_c _ _) _) =
  throwError "Application to non-function argument"
step (Step (Eq_c v_1 v_2) k) | v_1 == v_2 =
  return (k (ConV "true" []))
step (Step (Eq_c _ _) k) =
  return (k (ConV "false" []))
step (Step (Fail s) _) =
  throwError s

drive ::  Thread_c ConcreteValue ->
          ConcreteMonad ConcreteValue
drive (Stop x)  = return x
drive c         = do r <- step c; drive r

runSteps ::  Expr -> Env ConcreteValue ->
             Either String ConcreteValue
runSteps e nv = runExcept (runReaderT (drive (comp e)) nv)


