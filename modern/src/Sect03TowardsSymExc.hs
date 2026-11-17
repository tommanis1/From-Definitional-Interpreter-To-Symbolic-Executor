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

-- Free monad

data Free c a  =  Stop a
               |  forall b. Step (c b) (b -> Free c a)

liftF :: c a -> Free c a
liftF c = Step c Stop

instance Functor (Free c) where
  fmap f (Stop a) = Stop (f a)
  fmap f (Step c k) = Step c (fmap f . k)

instance Applicative (Free c) where
  pure = Stop
  (Stop f) <*> x = fmap f x
  (Step c k) <*> x = Step c (\y -> k y <*> x)

instance Monad (Free c) where
  Stop a >>= k = k a
  Step c f  >>= k  = Step c (\x -> f x >>= k)

-- Command type

data Cmd val :: * -> * where
  Fail   ::  String -> Cmd val a
