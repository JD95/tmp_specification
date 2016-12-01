module Instantiation (
  instantiate
) where

import Control.Arrow
import Data.List
import Control.Monad
import Data.Maybe
import Data.Ord

import Value
import MetaValue

instantiate :: MetaValue -> [Value] -> Maybe MetaValue
instantiate (Template mi ss) vs = f . catMaybes $ fmap (rankSpecialization vs) ss
  where f :: [(Int, MetaValue)] -> Maybe MetaValue
        f [] = Nothing
        f ms = Just . snd . maximumBy (comparing fst) $ ms
instantiate _ _ = Nothing

expandTLists :: Int -> [MetaArg] -> [MetaArg]
expandTLists size = concatMap f
  where f (Tlist _) = replicate size TlistArg
        f other = [other]

rankSpecialization :: [Value] -> ([MetaArg], MetaValue) -> Maybe (Int, MetaValue)
rankSpecialization vs (margs, mv)
  | length margs /= length vs = Nothing
  | otherwise = pure . flip (,) mv
            <=< foldr (\x y -> pure (+) <*> x <*> y) (Just 0)
              . zipWith comprArg (expandTLists (length vs - length margs) margs)
              $ vs

  where comprArg :: MetaArg -> Value -> Maybe Int
        comprArg TlistArg _ = Just 1
        comprArg (Targ _) (IntLit _) = Nothing
        comprArg (Targ _) (BoolLit _) = Nothing
        comprArg (Targ _) _ = Just 2
        comprArg (Tint _ _) (IntLit _) = Just 4
        comprArg (Tbool _ _) (BoolLit _) = Just 4
        comprArg _ _ = Nothing
