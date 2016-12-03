
import Test.QuickCheck

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
import Lib

program = do
  define . InMetaValue . lined $ template "add" $
     spec [arg_ Tint "x", arg_ Tint "y"] $ lined .
       group "add" $ do
         single (InExpr $ tmp_ Tint "x") "value"

  define . InMetaValue . lined $ template "addN" $
    spec [arg_ Tint "N"] $ lined . group "addN" $ do
      single (InExpr $ tmp_ Tint "N") "value"
      -- template "func" $ spec [arg_ Tint "x"] $ lined $
      --  single (InExpr $ (usrVal_ "add" <:> [tmp_ Tint "x", val_ $ IntLit 1]).:"value") "result"

      single (InExpr $ (usrVal_ "add" <:> [val_ $ IntLit 1, val_ $ IntLit 1]).:"value") "result"

add = usrVal_ "add"

addN = usrVal_ "addN"

getAddValue args = evalExpr ((add <:> args).: "value")

getAddNResult x y =
  evalExpr(((addN <:> [val_ $ IntLit x]).:"result"))

compileTest p t = evalSymbols (Map.fromList []) (p >> t)

betaReductionTest i j = isRight
                      . compileTest program
                      . getAddValue $ [val_ $ IntLit i, val_ $ IntLit j]

nestingExpr i j = isRight
                . compileTest program
                . getAddValue $
                  [ (add <:> [val_ $ IntLit i, val_ $ IntLit j]).: "value"
                  , (add <:> [val_ $ IntLit i, val_ $ IntLit j]).: "value"
                  ]

tooFewTempArgs = isLeft
               . compileTest program
               . getAddValue $ []


tooManyTempArgs = isLeft
                . compileTest program
                . getAddValue $ [ val_ $ IntLit 0
                                , val_ $ IntLit 0
                                , val_ $ IntLit 0
                                ]

wrongTempArgType = isLeft
                 . compileTest program
                 . getAddValue $ [val_ VOID, val_ VOID]

innerExprBetaReduction = isRight
                       . compileTest program
                       $ getAddNResult 1 1


main :: IO ()
main = do
  -- quickCheck betaReductionTest
  -- quickCheck nestingExpr
  -- quickCheck tooFewTempArgs
  -- quickCheck tooManyTempArgs
  -- quickCheck wrongTempArgType
  -- quickCheck innerExprBetaReduction
  print (compileTest program $ getAddNResult 1 1)
