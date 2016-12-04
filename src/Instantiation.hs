{-# LANGUAGE ExistentialQuantification #-}

module Instantiation (
  instantiate,
  matchTArgs
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


betaLookup :: forall a. Either MetaArg Value -> BetaReduction FExpr
betaLookup (Right v) = return . Right . inFExpr $ Type (Right v)
betaLookup (Left marg) = get >>= \maps -> case lookup marg maps of
              Just PLACEHOLD -> return . Right . inFExpr $ Type (Left marg)
              Just v -> return . Right . inFExpr $ Type (Right v)
              Nothing -> return . Left $ "Templated type was not instantiated!"

reduceExpr :: Id -> FExpr -> BetaReduction FExpr
reduceExpr i = outExpr >>> F.cata f
  where f :: Expr (BetaReduction FExpr) -> BetaReduction FExpr
        f (Type t) = betaLookup t

        f (Scope e i) = get >>= \maps ->
          return . fmap (inFExpr . flip Scope i . outExpr) $ evalState e maps

        f (Instantiate e args) = get >>= \maps -> do
          let e' = evalState e maps
          let args' = sequence . fmap (`evalState` maps) $ args
          return . fmap (inFExpr . fmap outExpr)
                 $ pure Instantiate <*> e' <*> args'

        f (ADD x y) = get >>= \maps -> do
          let x' = evalState x maps
          let y' = evalState y maps
          return . fmap (inFExpr . fmap outExpr)
                 $ pure ADD <*> x' <*> y'

reduceLine :: [(MetaArg, Value)] -> Line (BetaReduction a) () -> Either String a
reduceLine maps (Line br _) = evalState br maps

repackGroup :: Id -> [F.Fix MetaValue] -> FMetaValue
-- ^ Packs a list of meta values into a group with a Free monad again
repackGroup i = inFMetaValue . Group i

reduceTemplate :: [(MetaArg, Value)] -> ([MetaArg], BetaReduction a) -> Either String a
reduceTemplate maps = uncurry (flip evalState) . first ((++) maps . fmap (flip (,) PLACEHOLD))

repackTemplate i args = inFMetaValue . Template i . zip args

getTypeValue :: FExpr -> Either String (Either MetaArg Value)
getTypeValue = outExpr >>> F.unfix >>> f
  where f (Type v) = Right v
        f _ = Left "Is not a type"

dummy :: BetaReduction FMetaValue
dummy = return . Right . inFMetaValue $ Single (Id "") (InExpr $ val_ PLACEHOLD)

exprDummy :: BetaReduction FExpr
exprDummy = return . Right . inFExpr $ Type (Right PLACEHOLD)

betaReduce :: [(MetaArg, Value)] -> F.Fix MetaValue -> Either String FMetaValue
betaReduce maps = F.cata f >>> flip evalState maps
  where f :: MetaValue (BetaReduction FMetaValue) -> BetaReduction FMetaValue

        -- If is a single template value, replace it with the mapped Value.
        f (Single i v) = get >>= \maps ->
          return . fmap (inFMetaValue . Single i)
                 . (`evalState` maps) $ reduceExpr i  v

        -- Unpack group and reduce each line, repacking the group if each
        -- line has a successful reduction.
        f (Group i ls) = get >>= \maps ->
          return . fmap (repackGroup i . fmap outMetaValue)
                 . sequence
                 . fmap (`evalState` maps) $ ls

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
        comprArg (Tint _) (IntLit _) = Right 4
        comprArg (Tbool _) (BoolLit _) = Right 4
        comprArg _ _ = Left "Template arg mismatch"

expandPack :: Value -> [Value]
expandPack (PACK [v]) = [v]
expandPack (PACK vs)= vs
expandPack v = [v]

matchTArgs :: [MetaArg] -> [Value] -> Either String [(MetaArg, Value)]
matchTArgs margs ts =
  let vs = concatMap expandPack ts in
  case break isTlist margs of
    (ts, [])  -> if length ts == length vs
                 then Right (zip ts vs)
                 else Left "Wrong number of template Argumenets"
    (ts, [lst]) -> case splitAt (length ts) vs of
                   (vs', []) -> Right (zip ts vs)
                   (vs', lstVs) -> let pack = [(lst, PACK lstVs)] in
                                   Right (zip ts vs' ++ pack)
