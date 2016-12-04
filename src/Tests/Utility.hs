module Tests.Utility where

import Control.Monad.Free
import Control.Arrow
import Control.Monad
import Data.Either
import qualified Data.Map as Map
import qualified Data.Functor.Foldable as F

import Value
import MetaValue
import Symbols
import Expr
import MathExpr
import BooleanExpr
import Lib
import Instantiation

define' = define . InMetaValue . lined
compileTest p t = evalSymbols (Map.fromList []) (p >> t)
printResult (Right r) = print r
printResult (Left l) = print l
