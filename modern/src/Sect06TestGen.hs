module Sect06TestGen where

import Sect02SemStd
import Sect04SymExc
import Lib
import Sugar

-- Example smart constructors for interpreted language

lambda :: String -> Expr -> Expr
lambda x e = Con "lambda" [Con ("__" ++ x) [], e] -- atoms as vars

plambda :: Patt -> Patt -> Patt
plambda x e = PCon "lambda" [x, e]

ap :: Expr -> Expr -> Expr
ap f a = Con "app" [f, a]

papp :: Patt -> Patt -> Patt
papp f a = PCon "app" [f, a]

kvar :: String -> Expr -- atoms as vars
kvar x = Con "var" [Con ("__" ++ x) []]

pkvar :: Patt -> Patt
pkvar p = PCon "var" [p]

closv :: Expr -> Expr -> Expr -> Expr
closv x e nv = Con "closv" [x, e, nv]

pclosv :: Patt -> Patt -> Patt -> Patt
pclosv x e nv = PCon "closv" [x, e, nv]

-- Interpreter stubs

eval :: Expr
eval = error "Not implemented"

evil :: Expr
evil = error "Not implemented"
