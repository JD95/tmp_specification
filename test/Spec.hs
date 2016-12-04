
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
import MathExpr
import BooleanExpr
import Lib
import Instantiation

basicMathOp f name = InMetaValue . lined $ template name $
   spec [arg_ Tint "x", arg_ Tint "y"] $ lined .
     group name $ single (InExpr $ f (tmp_ Tint "x") (tmp_ Tint "y")) "value"

define' = define . InMetaValue . lined

program = do
  define $ basicMathOp (+:) "add"
  define $ basicMathOp (-:) "sub"
  define $ basicMathOp (*:) "mul"
  define $ basicMathOp (/:) "div"

  define' $ template "addN" $
    spec [arg_ Tint "N"] $ lined . group "addN" $ do
      single (InExpr $ tmp_ Tint "N") "value"
      template "result" $ spec [arg_ Tint "x"] $ lined $
        single (InExpr $ (usrVal_ "add" <:> [tmp_ Tint "x", val_ $ IntLit 1]).: "value") "result"

  define . InMetaValue . lined $ template "list" $ do

--  template <class H, class ...T>
    spec [arg_ Targ "H", arg_ Tlist "T"]$ lined . group "list" $ do
--    using head = H;
      single (InExpr $ tmp_ Targ "H") "head"
--    using tail = list<T...>;
      single (InExpr $ usrVal_ "list" <:> [tmp_ Tlist "T"]) "tail"

    spec [arg_ Targ "H"]$ lined . group "list" $
      single (InExpr $ tmp_ Targ "H") "head"

getAddValue args =
  evalExpr' ((usrVal_ "add" <:> args).:"value")

getAddNResult x y =
  evalExpr'(((usrVal_ "addN" <:> [val_ $ IntLit x]).:"result"<:> [val_ $ IntLit y]))

makeList args =
  evalExpr' (usrVal_ "list" <:> args)

makeAndGetTail args =
  evalExpr (Id "bob") (((usrVal_ "list" <:> args).:"tail").:"tail")

compileTest p t = evalSymbols (Map.fromList []) (p >> t)

betaReductionTest i j = isRight
                      . compileTest program
                      . getAddValue $ [val_ $ IntLit i, val_ $ IntLit j]

nestingExpr i j = isRight
                . compileTest program
                . getAddValue $
                  [ (usrVal_ "add" <:> [val_ $ IntLit i, val_ $ IntLit j]).: "value"
                  , (usrVal_ "add" <:> [val_ $ IntLit i, val_ $ IntLit j]).: "value"
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

typePackWorks = isRight
              . compileTest program
              $ makeList [val_ INT, val_ CHAR, val_ BOOL]

typePackSpec = isRight
             . compileTest program
             $ makeAndGetTail [val_ INT, val_ CHAR, val_ BOOL]

printResult (Right r) = print r
printResult (Left l) = print l

main :: IO ()
main = do
  quickCheck betaReductionTest
  quickCheck nestingExpr
  quickCheck tooFewTempArgs
  quickCheck tooManyTempArgs
  quickCheck wrongTempArgType
  quickCheck innerExprBetaReduction
  quickCheck typePackWorks
  quickCheck typePackSpec
  -- printResult (compileTest program $ makeAndGetTail [val_ INT, val_ CHAR, val_ BOOL])
  -- printResult (compileTest program $ getAddNResult 1 1)
