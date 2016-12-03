module Lib(
  someFunc,
  lined
) where

import Control.Monad.Free
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Functor.Foldable as F

import Value
import MetaValue
import Symbols
import Expr

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Stmt a = Using Id FExpr

lined :: Free (Line a) () -> a
lined (Free (Line a _)) = a

-- TODO Make math and bool operators
