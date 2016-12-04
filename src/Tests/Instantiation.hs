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
   spec [arg_ Tint "x", arg_ Tint "y"] $ lined .
     group name $ single (InExpr $ f (tmp_ Tint "x") (tmp_ Tint "y")) "value"

program = do
  define $ basicMathOp (+:) "add"
  define $ basicMathOp (-:) "sub"
  define $ basicMathOp (*:) "mul"
  define $ basicMathOp (/:) "div"

  define' $ template "addN" $
    spec [arg_ Tint "N"] $ lined . group "addN" $ do
      single (InExpr $ tmp_ Tint "N") "value"
      template "result" $ spec [arg_ Tint "x"] $ lined $
        single (InExpr $ usrT_ "add" [tmp_ Tint "x", val_ $ IntLit 1].: "value") "result"


getAddValue args =
  evalExpr' (usrT_ "add" (fmap val_ args).:"value")

getAddNResult x y =
  evalExpr'(usrT_ "addN" [val_ $ IntLit x].:"result"<:> [val_ $ IntLit y])

betaReductionTest i j = isRight
                      . compileTest program
                      . getAddValue $ [IntLit i, IntLit j]

tooFewTempArgs = isLeft
               . compileTest program
               . getAddValue $ []


tooManyTempArgs = isLeft
                . compileTest program
                . getAddValue $ [IntLit 0, IntLit 0, IntLit 0]

wrongTempArgType = isLeft
                 . compileTest program
                 . getAddValue $ [VOID, VOID]

innerExprBetaReduction = isRight
                       . compileTest program
                       $ getAddNResult 1 1
