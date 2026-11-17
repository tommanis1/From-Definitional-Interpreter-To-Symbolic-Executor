{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Sect05Correctness where

import Sect02SemStd

data SymbolicValue where      -- fake
data Thread_s :: * -> * where -- fake
data Config_s :: * -> * where -- fake

-- runSteps_s ::  Expr -> Env SymbolicValue -> ...
