module Tests.List (listTests) where

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

import Tests.Utility

listTests = do
  quickCheck typePackWorks
  quickCheck typePackSpec

program = do
  define . InMetaValue . lined $ template "list" $ do
--  template <class H, class ...T>
    spec [arg_ Targ "H", arg_ Tlist "T"]$ lined . group "list" $ do
--    using head = H;
      single (InExpr $ tmp_ Targ "H") "head"
--    using tail = list<T...>;
      single (InExpr $ usrT_ "list" [tmp_ Tlist "T"]) "tail"

    spec [arg_ Targ "H"]$ lined . group "list" $
      single (InExpr $ tmp_ Targ "H") "head"

makeList args =
  evalExpr' (usrT_ "list" (fmap val_ args))

makeAndGetTail args =
  evalExpr (Id "bob") (usrT_ "list" (fmap val_ args).:"tail".:"tail".:"head")

typePackWorks = isRight
              . compileTest program
              $ makeList [INT, CHAR, BOOL]

typePackSpec = isRight
             . compileTest program
             $ makeAndGetTail [INT, CHAR, BOOL]
