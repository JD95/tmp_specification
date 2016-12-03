
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
     spec [arg_ Tint "x", arg_ Tbool "y"] $ lined .
       group "add" $
         single (InExpr $ tmp_ Tint "x") "value"
  -- define . InMetaValue . lined $ group "hasAdd" $
  --   single ()

add = usrVal_ "add"

getAddValue args = evalExpr ((add <:> args).: "value")

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

main :: IO ()
main = do
  quickCheck betaReductionTest
  quickCheck nestingExpr
  quickCheck tooFewTempArgs
  quickCheck tooManyTempArgs
  quickCheck wrongTempArgType
