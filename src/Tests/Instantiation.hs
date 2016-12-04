module Tests.Instantiation (instantiationTests) where

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

import Tests.Utility
import Test.QuickCheck

instantiationTests = do
  quickCheck betaReductionTest
  quickCheck tooFewTempArgs
  quickCheck tooManyTempArgs
  quickCheck wrongTempArgType
  quickCheck innerExprBetaReduction

basicMathOp f name = InMetaValue . lined $ template name $
   spec [int_ "x", int_ "y"] $ lined .
     group name $ single (InExpr $ f (tmp_ Tint "x") (tmp_ Tint "y")) "value"

program = do
  define $ basicMathOp (+:) "add"
  define $ basicMathOp (-:) "sub"
  define $ basicMathOp (*:) "mul"
  define $ basicMathOp (/:) "div"

  define' $ template "addN" $
--  template <int N> struct addN {
    spec [int_ "N"] $ lined . group "addN" $ do
--    static constexpr int value = N;
      single (InExpr $ tmp_ Tint "N") "value"

--    template <int x>
      template "result" $ spec [int_ "x"] $ lined $
--      using result = add<x,1>::value;
        single (InExpr $ usrT_ "add" [tmp_ Tint "x", val_ (IntLit 1)]) "result"
--};

-- FOR DEMOS ------------------------------------------------------------------
binaryMathOp :: String -> Int -> Int -> IO ()
binaryMathOp name x y =
  printResult . compileTest program
              . evalExpr'
              $ (usrT_ name [val_ $ IntLit x, val_ $ IntLit y].:"value")

add = binaryMathOp "add"
sub = binaryMathOp "sub"
mult = binaryMathOp "mul"
divide = binaryMathOp "div"
-------------------------------------------------------------------------------

getAddValue args =
  evalExpr' (usrT_ "add" (fmap val_ args).:"value")

getAddNResult x y =
  evalExpr'(usrT_ "addN" [val_ (IntLit x)].:"result"<:> [val_ (IntLit y)])

betaReductionTest i j = isRight
                      . compileTest program
                      . getAddValue $ [IntLit i, IntLit j]

-- Attempts to call a meta function with too few arguments
tooFewTempArgs = isLeft
               . compileTest program
               . getAddValue $ []

-- Attempts to call a meta function with too many arguments
tooManyTempArgs = isLeft
                . compileTest program
                . getAddValue $ [IntLit 0, IntLit 0, IntLit 0]

-- Attempts to call a meta function with arguments of the wrong type
wrongTempArgType = isLeft
                 . compileTest program
                 . getAddValue $ [VOID, VOID]

innerExprBetaReduction = isRight
                       . compileTest program
                       $ getAddNResult 1 1
