{-# LANGUAGE DeriveFunctor #-}

module MetaValue(
  MetaValue(..),
  Line(..),
  listFromLines,
  collapseGroupMembers,
  metaId,
  single,
  group,
  template
) where

import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Free
import Data.List (nub, intercalate, find)

import Value

data MetaValue = Template Id [([MetaArg], MetaValue)]
               | Group Id (Free (Line MetaValue) ()) -- ^ A struct or record
               | Single Id Value -- ^ static const int value = 5;

data Line a next = Line a next
              | EndLine
                deriving (Functor)

listFromLines :: Line a [a] -> [a]
listFromLines (Line r rs) = r:rs
listFromLines EndLine = []

collapseGroupMembers = iter listFromLines . fmap (const [])

metaId (Template i _ ) = i
metaId (Group i _) = i
metaId (Single i _) = i

showCommaList :: Show a => [a] -> String
showCommaList = intercalate "," . map show

showGroupLines g@(Single i v) = [show g]
showGroupLines g@(Template _ specializations) = [show g]
showGroupLines (Group i vs) = ["struct " ++ show i ++ "{"] ++ members ++ ["};"]
  where members = concatMap (fmap ("\t" ++) . showGroupLines) . collapseGroupMembers $ vs

instance Show MetaValue where
  show (Single i v) = show v ++ " " ++ show i
  show (Group i vs) = "struct " ++ show i ++ "{\n" ++ concat members ++ "};\n"
    where members = concatMap (fmap (\m -> "\t" ++ m ++ "\n") . showGroupLines) $ collapseGroupMembers vs
  show (Template i ss) = "template<" ++ showCommaList ss ++ "> " ++ show i

single v i = Free (Line (Single (Id i) v) (Pure ()))

group i vs = Free (Line (Group (Id i) vs) (Pure ()))

template i s = Free (Line (Template (Id i) s) (Pure ()))
