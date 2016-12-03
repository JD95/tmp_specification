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
import Instantiation

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


groupMemberFind :: Id -> [FMetaValue] -> Either String FMetaValue
groupMemberFind i ms = case find ((==) i. metaId) ms of
  Just m -> Right m
  Nothing -> Left $ show i ++ " undefined!"

evalScope :: Id -> FMetaValue -> Symbols FMetaValue
evalScope i = outMetaValue >>> F.unfix >>> f
  where f (Template mi _) = symbolError $ "Template " ++ show mi ++ " must be instantiated before inner types can be used!"
        f (Single mi _) = symbolError $ show i ++ " has no types to resolve!"
        f (Group _ vs) = Lookup . const . groupMemberFind i . fmap InMetaValue $ vs

evalExpr :: F.Fix Expr -> Symbols FMetaValue
evalExpr = F.cata f
  where f :: Expr (Symbols FMetaValue) -> Symbols FMetaValue
        -- Scope resolution
        f (Scope e i) =  e >>= evalScope i
        -- Template instantiation
        f (Instantiate e args) = Lookup $ flip instantiate args <=< flip evalSymbols e
        -- A single type
        f (Type (USR t)) = lookupId t
        f (Type t) = pure (inFMetaValue (Single (Id . show $ t) (Right t)))
