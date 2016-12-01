module Value(
  Value(..),
  Id(..),
  MetaArg(..),
  class_,
  list_,
  isTlist
) where

import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Free
import Data.List (nub, intercalate, find)

newtype Id = Id String deriving (Eq, Ord)

instance Show Id where
  show (Id s) = s

data Value = INT
           | IntLit Int
           | FLOAT
           | DOUBLE
           | CHAR
           | UINT
           | SHORT
           | LONG
           | VOID
           | BOOL
           | BoolLit Bool
           | CONST Value
           | PTR Value
           | REF Value
           | STATIC Value
           | USR Id
             deriving (Eq, Ord)


instance Show Value where
  show INT = "int"
  show (IntLit a) = show a
  show FLOAT = "float"
  show DOUBLE = "double"
  show CHAR = "char"
  show UINT = "uint"
  show SHORT = "short"
  show LONG = "long"
  show VOID = "void"
  show BOOL = "bool"
  show (BoolLit a) = show a
  show (CONST a) = "const " ++ show a
  show (PTR a) = show a ++ "*"
  show (STATIC a) = "static " ++ show a
  show (USR t) = show t

data MetaArg = Targ Id
             | Tint Id Int
             | Tbool Id Bool
             | Tlist Id
             | TlistArg

-- Makes the definitions look nicer
class_ t = Targ (Id t)
list_ t = Tlist (Id t)

isTlist (Tlist _) = True
isTlist _ = False

instance Show MetaArg where
  show (Targ t) = "class " ++ show t
  show (Tlist t) = "class ..." ++ show t
  show TlistArg = "listArg"
  show (Tint mi i) = "int " ++ show i
  show (Tbool mi b) = "bool " ++ show b
