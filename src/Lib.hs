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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Stmt a = Using Id FExpr

program = evalSymbols (Map.fromList []) $ do
  define $ Group (Id "testClass") $ do
    single INT "x"
    template "add" [class_ "T1", class_ "T2"] $ \args -> case args of
        [IntLit x, IntLit y] -> Right $
          Group (Id "add") $ single (IntLit (x + y)) "value"
        _ -> Left "Add must have args of type int literal"
    group "testSubClass" $ do
      single DOUBLE "otherVar"
  let testClass = type_ (USR (Id "testClass"))
  -- testClass::add<5,5>::value
  evalExpr ((testClass.:"add"<:>[IntLit 5, IntLit 5]).: "value")
