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

betaReduce :: [(MetaArg, Value)] -> FMetaValue -> Either String FMetaValue
betaReduce maps = outMetaValue >>> F.cata f >>> flip evalState maps
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
  fmap InMetaValue . f . catMaybes $ fmap (rankSpecialization vs) ss
  where f :: [(Int, z)] -> Either String z
        f [] = Left $ "No matching instance for " ++ show mi
        f ms = Right . snd . maximumBy (comparing fst) $ ms
instantiate mv _ = Left ("cannot instantiate " ++ show (metaId mv))

matchTArgs :: [MetaArg] -> [Value] -> Maybe [(MetaArg, Value)]
matchTArgs margs vs
  | length margs /= length vs = Nothing
  | otherwise = Just $ zip ts vs' ++ [(head lst, PACK lstVs)]
    where (ts, lst) = break isTlist margs
          (vs', lstVs) = splitAt (length ts - 1) vs

rankSpecialization :: [Value] -> ([MetaArg], z) -> Maybe (Int, z)
rankSpecialization vs (margs, mv) = pure . flip (,) mv
                                =<< foldr (\x y -> pure (+) <*> x <*> y) (Just 0)
                                =<< Just (fmap (uncurry comprArg))
                                <*> matchTArgs margs vs

  where comprArg :: MetaArg -> Value -> Maybe Int
        comprArg (Tlist _) (PACK _) = Just 1
        comprArg (Targ _) (IntLit _) = Nothing
        comprArg (Targ _) (BoolLit _) = Nothing
        comprArg (Targ _) _ = Just 2
        comprArg (Tint _ _) (IntLit _) = Just 4
        comprArg (Tbool _ _) (BoolLit _) = Just 4
        comprArg _ _ = Nothing
