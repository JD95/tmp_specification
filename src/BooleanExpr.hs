module BooleanExpr (

) where

import qualified Data.Functor.Foldable as F
import Control.Monad

import MetaValue
import Symbols
import Value

negation :: Value -> Either String Value
negation (BoolLit b) = Right $ BoolLit (not b)
negation other = Left $ "Cannot take negation of " ++ show other

logicalAnd :: Value -> Value -> Either String Value
logicalAnd (BoolLit x) (BoolLit y) = Right $ BoolLit (x && y)
logicalAnd _ _ = Left "The operation 'and' requires booleans"

and_ x y = F.Fix $ ADD x y
(&&:) = and_

logicalOr :: Value -> Value -> Either String Value
logicalOr (BoolLit x) (BoolLit y) = Right $ BoolLit (x || y)
logicalOr _ _ = Left "The operation 'or' requires booleans"

or_ x y = F.Fix $ ADD x y
(||:) = or_

equality :: Value -> Value -> Either String Value
equality (IntLit x) (IntLit y) = Right $ BoolLit (x == y)
equality (BoolLit x) (BoolLit y) = Right $ BoolLit (x == y)
equality x y = Left $ "Cannot check equality for " ++ show x ++ " and " ++ show y

eq_ x y = F.Fix $ ADD x y
(==:) = eq_

greaterThan :: Value -> Value -> Either String Value
greaterThan (IntLit x) (IntLit y) = Right $ BoolLit (x > y)
greaterThan x y = Left $ "Cannot compare " ++ show x ++ " and " ++ show y

gt_ x y = F.Fix $ ADD x y
(<:) = gt_

lessThan :: Value -> Value -> Either String Value
lessThan x y = join $ pure logicalAnd <*> notGT <*> notEQ
  where notGT = join $ pure negation <*> greaterThan x y
        notEQ = join $ pure negation <*> equality x y
