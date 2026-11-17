{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

module Sect04SymExc where

import Sect02SemStd
import Sect03TowardsSymExc
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fail as Fail
import Data.Tuple.HT
import ListMonad
import Lib
import Sugar
import Debug.Trace

-- Symbolic Values

data SymbolicValue  =  ConV'   String [SymbolicValue]
                    |  SymV    String

-- Monad types

type Unifier   = [(String, SymbolicValue)]
type Unifier_N = [(SymbolicValue, SymbolicValue)]
type SymbolicMonad = ReaderT () (ExceptT String IO)
type SymbolicSetMonad =
  StateT (Unifier, Unifier_N) (ListT SymbolicMonad)
