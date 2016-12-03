{-# LANGUAGE DeriveFunctor #-}

module MetaValue(
  FMetaValue(..),
  MetaValue(..),
  Line(..),
  listFromLines,
  collapseGroupMembers,
  metaId,
  single,
  group,
  template,
  inFMetaValue
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

data MetaValue a = Template Id [([MetaArg], a)]
                 | Group Id (Free (Line a) ()) -- ^ A struct or record
                 | Single Id (Either MetaArg Value) -- ^ static const int value = 5;\

instance Functor MetaValue where
  fmap f (Template i ss) = Template i (fmap (second f) ss)
  fmap f (Group i ls) = Group i (hoistFree (Bi.first f) ls)

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

collapseGroupMembers = iter listFromLines . fmap (const [])

metaId = outMetaValue >>> F.unfix >>> f
  where f (Template i _ ) = i
        f (Group i _) = i
        f (Single i _) = i

showCommaList :: Show a => [a] -> String
showCommaList = intercalate "," . map show

-- showGroupLines :: MetaValue [String] -> [String]
-- showGroupLines g@(Single i v) = [show g]
-- showGroupLines g@(Template _ specializations) = [show g]
-- showGroupLines (Group i vs) = ["struct " ++ show i ++ "{"] ++ members ++ ["};"]
--   where members = concatMap (fmap ("\t" ++) . showGroupLines) . collapseGroupMembers $ vs

instance Show FMetaValue where
  show = outMetaValue >>> F.cata f
    where f (Single i v) = show v ++ " " ++ show i
          f (Group i vs) = "struct " ++ show i ++ "{\n" ++ members ++ "};\n"
            where members = concatMap ((\m -> "\t" ++ m ++ "\n") . show) $ collapseGroupMembers vs
          f (Template i ss) = "template<" ++ showCommaList ss ++ "> " ++ show i

single v i = Free (Line (Single (Id i) v) (Pure ()))

group i vs = Free (Line (Group (Id i) vs) (Pure ()))

template i s = Free (Line (Template (Id i) s) (Pure ()))
