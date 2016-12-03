module Lib(
  someFunc
) where

import Control.Arrow
import Control.Monad
import qualified Data.Map as Map


import Value
import MetaValue
import Symbols
import Expr

import Control.Monad.Free

import qualified Data.Functor.Foldable as F

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Stmt a = Using Id FExpr

lined :: Free (Line a) () -> a
lined (Free (Line a _)) = a

-- TODO Make math and bool operators

program = evalSymbols (Map.fromList []) $ do
  define . InMetaValue . lined $ group "testClass" $ do
     single (Right INT) "x"
     template "add" $ do
        spec [Tint (Id "x"), Tbool (Id "y")] (Group (Id "add") [F.Fix $ Single (Id "value") (Right $ IntLit 0) ])
     group "testSubClass" $ do
        single (Right DOUBLE) "otherVar"

  let testClass = type_ (USR (Id "testClass"))
  -- testClass::add<5,5>::value
  evalExpr ((testClass.:"add"<:>[IntLit 5, IntLit 5]).: "value")
