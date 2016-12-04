{-# LANGUAGE DeriveFunctor #-}

module Value(
  Value(..),
  Id(..),
  MetaArg(..),
  class_,
  list_,
  isTlist,
  metaArgId,
  FMetaValue(..),
  MetaValue(..),
  Line(..),
  listFromLines,
  collapseLines,
  metaId,
  single,
  group,
  template,
  inFMetaValue,
  toFreeLine,
  spec,
  tmp_,
  val_,
  arg_,
  usrVal_,
  usrT_,
  Expr(..),
  FExpr(..),
  getType,
  inFExpr,
  getValue
) where

import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Free
import Data.List (nub, intercalate, find)
import qualified Data.Bifunctor as Bi
import Data.Functor.Classes

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
           | USRT Id [F.Fix Expr] -- Templated Type
           | PACK [Value]
           | PLACEHOLD  -- Needed for template instantiation
             deriving (Eq)

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
  show (USRT i ts) = show i ++ "<" ++ showCommaList (fmap InExpr ts) ++ ">"
  show (PACK ts) = show ts

data MetaArg = Targ Id
             | Tint Id
             | Tbool Id
             | Tlist Id
             deriving (Ord)

instance Eq MetaArg where
  (==) a b = metaArgId a == metaArgId b

metaArgId (Targ i) = i
metaArgId (Tint i) = i
metaArgId (Tbool i) = i
metaArgId (Tlist i) = i

-- Makes the definitions look nicer
class_ t = Targ (Id t)
list_ t = Tlist (Id t)

isTlist (Tlist _) = True
isTlist _ = False

instance Show MetaArg where
  show (Targ t) = "class " ++ show t
  show (Tlist t) = "class ..." ++ show t
  show (Tint mi) = "int "
  show (Tbool mi) = "bool "

data Expr a = Scope a Id -- someClass::value
            | Instantiate a [a] -- add<1,2>
            | ADD a a
            | SUB a a
            | MUL a a
            | DIV a a
            | NOT a
            | AND a a
            | OR a a
            | EQUALS a a
            | GRT a a
            | Type (Either MetaArg Value)
              deriving (Functor, Eq)

instance Eq1 Expr where
  liftEq _ (Type (Right a)) (Type (Right b)) = a == b
  liftEq _ (Type (Left a)) (Type (Left b)) = a == b
  liftEq eq (Scope a x) (Scope b y) = eq a b && x == y
  liftEq eq (Instantiate a as) (Instantiate b bs) = eq a b && liftEq eq as bs
  liftEq eq (ADD a b) (ADD c d) = eq a c && eq b d
  liftEq eq (SUB a b) (SUB c d) = eq a c && eq b d
  liftEq eq (MUL a b) (MUL c d) = eq a c && eq b d
  liftEq eq (DIV a b) (DIV c d) = eq a c && eq b d
  liftEq eq (NOT a) (NOT b) = eq a b
  liftEq eq (AND a b) (AND c d) = eq a c && eq b d
  liftEq eq (OR a b) (OR c d) = eq a c && eq b d
  liftEq eq (EQUALS a b) (EQUALS c d) = eq a c && eq b d
  liftEq eq (GRT a b) (GRT c d) = eq a c && eq b d
  liftEq _ _ _ = False

--   Fixed Expression
data FExpr = InExpr { outExpr::F.Fix Expr }

outFExpr = F.unfix . outExpr

instance Eq FExpr where
  a == b = outFExpr a == outFExpr b

inFExpr = InExpr . F.Fix

getType :: FExpr -> Either String Value
getType = f . F.unfix . outExpr
  where f (Type (Right v)) = Right v
        f (Type (Left v)) = Left $ "Template arg " ++ show v ++ " was not reduced!"
        f _ = Left "Expression does not evaluate to a value!"

getValue :: FMetaValue -> Either String Value
getValue mv = f . F.unfix . outMetaValue $ mv
  where f (Single e v) = getType v
        f _ = Left $ show (metaId mv) ++ " is not a value!"

instance Show FExpr where
  show = outExpr >>> F.cata f
    where f (Scope a (Id i)) = a ++ "::" ++ i
          f (Instantiate e args) = e ++ "<" ++ showCommaList args ++ ">"
          f (Type (Left m)) = show m
          f (Type (Right m)) = show m

data MetaValue a = Template Id [([MetaArg], a)]
                 | Group Id [a] -- ^ A struct or record
                 | Single Id FExpr -- ^ static const int value = 5;\

instance Functor MetaValue where
  fmap f (Template i ss) = Template i (fmap (second f) ss)
  fmap f (Group i ls) = Group i (fmap f ls)
  fmap f (Single i v) = Single i v


newtype FMetaValue = InMetaValue { outMetaValue :: F.Fix MetaValue }

inFMetaValue = InMetaValue . F.Fix

data Line a next = Line a next
              | EndLine
                deriving (Functor)

instance Bi.Bifunctor Line where
  bimap f g (Line a b) = Line (f a) (g b)

listFromLines :: Line a [a] -> [a]
listFromLines (Line r rs) = r:rs
listFromLines EndLine = []

collapseLines = iter listFromLines . fmap (const [])

toFreeLine :: a -> Free (Line a) ()
toFreeLine a = Free (Line a (Pure ()))

metaId = outMetaValue >>> F.unfix >>> f
  where f (Template i _ ) = i
        f (Group i _) = i
        f (Single i _) = i

showCommaList :: Show a => [a] -> String
showCommaList = intercalate "," . map show

instance Show FMetaValue where
  show = outMetaValue >>> F.cata f
    where f (Single i v) = if show i == "" then show v else show v ++ " " ++ show i
          f (Group i vs) = "group " ++ show i ++ "{\n" ++ members ++ "};\n"
            where members = concatMap ((\m -> "\t" ++ m ++ "\n") . show) vs
          f (Template i ss) = concatMap (\(args, v) -> "template<" ++ showCommaList args ++ "> " ++ show v ++ "\n") ss

tmp_ t i = F.Fix $ Type (Left (t (Id i)))

val_ = F.Fix . Type . Right

usrVal_ i = F.Fix . Type $ Right (USR (Id i))

usrT_ i ts = F.Fix . Type $ Right (USRT (Id i) ts)

arg_ t i = t (Id i)

single v i = Free (Line (F.Fix $ Single (Id i) v) (Pure ()))

group i vs = Free (Line (F.Fix $ Group (Id i) (collapseLines vs)) (Pure ()))

template i s = Free (Line (F.Fix $ Template (Id i) (collapseLines s)) (Pure ()))

spec margs mv = Free (Line (margs, mv) (Pure ()))
