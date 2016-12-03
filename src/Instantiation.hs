{-# LANGUAGE ExistentialQuantification #-}

module Instantiation (
  instantiate
) where

import Control.Arrow
import Data.List
import Control.Monad
import Data.Maybe
import Data.Ord

import Value
import qualified Data.Functor.Foldable as F
import MetaValue

import Symbols
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bi

import Control.Monad.State.Lazy
import Control.Monad.Free

import Data.Either

-- TODO ACTUALLY DO THE REPLACEMENT OF TEMPLATE VALUES IN OBJ

type BetaReduction a = State [(MetaArg, Value)] (Either String a)

betaLookup :: forall a. Either MetaArg Value -> (Value -> a) -> BetaReduction a
betaLookup (Right v) a = return . Right $ a v
betaLookup (Left marg) a = get >>= \maps -> case lookup marg maps of
              Just v -> return . Right $ a v
              Nothing -> return . Left $ "Templated type was not instantiated!"

reduceLine :: [(MetaArg, Value)] -> Line (BetaReduction a) () -> Either String a
reduceLine maps (Line br _) = evalState br maps

repackGroup :: (Foldable t, Functor t) => Id -> t (F.Fix MetaValue) -> FMetaValue
-- ^ Packs a list of meta values into a group with a Free monad again
repackGroup i = inFMetaValue . Group i . foldr1 (>>)  . fmap toFreeLine

reduceTemplate :: [(MetaArg, Value)] -> ([MetaArg], BetaReduction a) -> Either String a
reduceTemplate maps = uncurry (flip evalState) . first ((++) maps . fmap (flip (,) VOID))

repackTemplate i args = inFMetaValue . Template i . zip args

betaReduce :: [(MetaArg, Value)] -> F.Fix MetaValue -> Either String FMetaValue
betaReduce maps = F.cata f >>> flip evalState maps
  where f :: MetaValue (BetaReduction FMetaValue) -> BetaReduction FMetaValue

        -- If is a single template value, replace it with the mapped Value.
        f (Single i v) = betaLookup v (inFMetaValue . Single i . Right)

        -- Unpack group and reduce each line, repacking the group if each
        -- line has a successful reduction.
        f (Group i ls) = get >>= \maps ->
          return . fmap (repackGroup i . fmap outMetaValue)
                 . sequence
                 . fmap (`evalState` maps)
                 . collapseGroupMembers $ ls

        -- Add the args to template as trivial mappings to evaluate template
        -- value and then repack the specializations if all reductions succeed.
        f (Template i ss) = get >>= \maps ->
          return . fmap (repackTemplate i (fmap fst ss) . fmap outMetaValue)
                 . sequence
                 . fmap (reduceTemplate maps) $ ss


instantiate :: FMetaValue -> [Value] -> Either String FMetaValue
instantiate (InMetaValue (F.Fix (Template mi ss))) vs =
        f . rights $ fmap (rankSpecialization vs) ss
  where f :: [(Int, [MetaArg],  F.Fix MetaValue)] -> Either String FMetaValue
        f [] = Left $ "No matching instance for " ++ show mi
        f ms = uncurry betaReduce
           <=< (\(_,m,z) -> (flip (,) z <$> matchTArgs m vs))
             . maximumBy (comparing (\(i,_,_) -> i)) $ ms
instantiate mv _ = Left ("cannot instantiate " ++ show (metaId mv))

matchTArgs :: [MetaArg] -> [Value] -> Either String [(MetaArg, Value)]
matchTArgs margs vs
  | length margs > length vs = Left "To few template args"
  | length margs < length vs = Left "To many template args"
  | otherwise = Right $ zip ts vs' ++ [(head lst, PACK lstVs)]
    where (ts, lst) = break isTlist margs
          (vs', lstVs) = splitAt (length ts - 1) vs

rankSpecialization :: [Value] -> ([MetaArg], z) -> Either String (Int, [MetaArg], z)
rankSpecialization vs (margs, mv) = pure . (\i -> (i, margs, mv))
                                =<< foldr (\x y -> pure (+) <*> x <*> y) (Right 0)
                                =<< Right (fmap (uncurry comprArg))
                                <*> matchTArgs margs vs

  where comprArg :: MetaArg -> Value -> Either String Int
        comprArg (Tlist _) (PACK _) = Right 1
        comprArg (Targ _) (IntLit _) = Left "Template arg mismatch"
        comprArg (Targ _) (BoolLit _) = Left "Template arg mismatch"
        comprArg (Targ _) _ = Right 2
        comprArg (Tint _ _) (IntLit _) = Right 4
        comprArg (Tbool _ _) (BoolLit _) = Right 4
        comprArg _ _ = Left "Template arg mismatch"
