{-# LANGUAGE DeriveFunctor #-}

module Expr(
  Expr(..),
  FExpr(..),
  (.:),
  (<:>),
  type_,
  groupMemberFind,
  evalExpr
) where

import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Free
import Data.List (nub, intercalate, find)

import Value
import MetaValue
import Symbols

data Expr a = Scope a Id -- someClass::value
            | Instantiate a [Value] -- add<1,2>
            | Type Value
              deriving (Functor)

--   Fixed Expression
data FExpr = InExpr { outExpr::F.Fix Expr }

scope_ e i = F.Fix $ Scope e (Id i)
(.:) = scope_

infixl 7 .:

instantiate_ e args = F.Fix $ Instantiate e args
(<:>) = instantiate_

infix 6 <:>

type_ t = F.Fix $ Type t

groupMemberFind :: Id -> [MetaValue] -> Either String MetaValue
groupMemberFind i ms = case find ((==) i. metaId) ms of
  Just m -> Right m
  Nothing -> Left $ show i ++ " undefined!"

evalExpr :: F.Fix Expr -> Symbols MetaValue
evalExpr = F.cata f
  where f :: Expr (Symbols MetaValue) -> Symbols MetaValue
        -- Scope resolution
        f (Scope e i) = e >>= \expr -> case expr of
          Template mi _ _ -> symbolError $ "Template " ++ show mi ++ " must be instantiated before inner types can be used!"
          Single mi _ -> symbolError $ show i ++ " has no types to resolve!"
          Group _ vs -> Lookup . const . groupMemberFind i . collapseGroupMembers $ vs
        -- Template instantiation
        f (Instantiate e args) = e >>= \expr -> case expr of
          Template mi tArgs f -> if isTlist (last tArgs) || length args == length tArgs
            then Lookup . const $ f args
            else symbolError $ "Type arguments do not match template " ++ show mi
          Single mi _ -> symbolError $ show mi ++ " is not a template!"
          Group mi vs -> symbolError $ show mi ++ " is not a template!"
        -- A single type
        f (Type (USR t)) = lookupId t
        f (Type t) = Lookup . const $ pure (Single (Id . show $ t) t)
