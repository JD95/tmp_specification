
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
import Instantiation

program = do
  define . InMetaValue . lined $ template "add" $
     spec [arg_ Tint "x", arg_ Tint "y"] $ lined .
       group "add" $ do
         single (InExpr $ tmp_ Tint "x") "value"

  define . InMetaValue . lined $ template "addN" $
    spec [arg_ Tint "N"] $ lined .
      group "addN" $ do
        single (InExpr $ tmp_ Tint "N") "value"
        template "result" $ spec [arg_ Tint "x"] $ lined $
          single (InExpr $ (usrVal_ "add" <:> [tmp_ Tint "x", val_ $ IntLit 1]).: "value") "result"

  define . InMetaValue . lined $ template "list" $
    spec [arg_ Targ "H", arg_ Tlist "T"]$ lined .
      group "list" $ do
        single (InExpr $ tmp_ Targ "H") "head"
        single (InExpr $ tmp_ Tlist "T") "tail"

getAddValue args =
  evalExpr' ((usrVal_ "add" <:> args).: "value")

getAddNResult x y =
  evalExpr'((usrVal_ "addN" <:> [val_ $ IntLit x]).:"result"<:> [val_ $ IntLit y])

makeList args =
  evalExpr' (usrVal_ "list" <:> args)

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

main :: IO ()
main = do
  quickCheck betaReductionTest
  quickCheck nestingExpr
  quickCheck tooFewTempArgs
  quickCheck tooManyTempArgs
  quickCheck wrongTempArgType
  quickCheck innerExprBetaReduction
  quickCheck typePackWorks
  print (matchTArgs [Targ (Id "H"), Tlist (Id "T")] [INT, CHAR, BOOL])
