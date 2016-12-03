module Expr(
  Expr(..),
  FExpr(..),
  (.:),
  (<:>),
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

scope_ e i = F.Fix $ Scope e (Id i)
(.:) = scope_

infixl 7 .:

instantiate_ e args = F.Fix $ Instantiate e args
(<:>) = instantiate_

infix 6 <:>

groupMemberFind :: Id -> [FMetaValue] -> Either String FMetaValue
groupMemberFind i ms = case find ((==) i. metaId) ms of
  Just m -> Right m
  Nothing -> Left $ show i ++ " undefined!"

evalScope :: Id -> FMetaValue -> Symbols FMetaValue
evalScope i = outMetaValue >>> F.unfix >>> f
  where f (Template mi _) = symbolError $ "Template " ++ show mi ++ " must be instantiated before inner types can be used!"
        f (Single mi _) = symbolError $ show i ++ " has no types to resolve!"
        f (Group _ vs) = Lookup . const . groupMemberFind i . fmap InMetaValue $ vs
evalArgs tbl = join
             . fmap (sequence . fmap getValue)
             . sequence
             . fmap (evalSymbols tbl)

evalExpr :: F.Fix Expr -> Symbols FMetaValue
evalExpr = F.cata f
  where f :: Expr (Symbols FMetaValue) -> Symbols FMetaValue
        -- Scope resolution
        f (Scope e i) =  e >>= evalScope i

        -- Template instantiation
        f (Instantiate e args) = Lookup $ \tbl -> do
          let e' = evalSymbols tbl e
          let args' = evalArgs tbl args
          join $ pure instantiate <*> e' <*> args'

        -- A single type
        f (Type (Right (USR t))) = lookupId t
        f (Type t) = pure (inFMetaValue (Single (Id . show $ t) (inFExpr $ Type t)))
