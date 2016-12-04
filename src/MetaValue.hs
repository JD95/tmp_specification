{-# LANGUAGE DeriveFunctor #-}

module MetaValue(
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

import Value

data Expr a = Scope a Id -- someClass::value
            | Instantiate a [a] -- add<1,2>
            | ADD a a
            | SUB a a
            | MUL a a
            | DIV a a
            | NOT a
            | AND a a
            | OR a a
            | EQ a a
            | GT a a
            | Type (Either MetaArg Value)
              deriving (Functor)

--   Fixed Expression
data FExpr = InExpr { outExpr::F.Fix Expr }

inFExpr = InExpr . F.Fix

getType :: FExpr -> Either String Value
getType = f . F.unfix . outExpr
  where f (Type (Right v)) = Right v
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

arg_ t i = t (Id i)

single v i = Free (Line (F.Fix $ Single (Id i) v) (Pure ()))

group i vs = Free (Line (F.Fix $ Group (Id i) (collapseLines vs)) (Pure ()))

template i s = Free (Line (F.Fix $ Template (Id i) (collapseLines s)) (Pure ()))

spec margs mv = Free (Line (margs, mv) (Pure ()))
