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

-- Syntax

data Expr     =  Con String [Expr]
              |  EEq Expr Expr
              |  Case Expr [(Patt, Expr)]
              |  Var String
              |  Lam String Expr
              |  App Expr Expr
              |  Let [(String, Expr)] Expr
              |  Letrec [(String, ValExpr)] Expr
              deriving Eq

data ValExpr  =  VCon String [ValExpr]
              |  VLam String Expr
              deriving Eq

data Patt     =  PVar String
              |  PCon String [Patt]
              deriving Eq

resolve :: String -> [(String, v)] -> v
resolve x nv = fromJust (lookup x nv)
