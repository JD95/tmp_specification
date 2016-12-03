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

groupMemberFind :: Id -> [FMetaValue] -> Symbols FMetaValue
groupMemberFind i ms = case find ((==) i. metaId) ms of
  Just m -> return m
  Nothing -> symbolError $ show i ++ " undefined!"

evalScope :: Id -> FMetaValue -> Symbols FMetaValue
evalScope i = outMetaValue >>> F.unfix >>> f
  where f (Template mi _) = symbolError $ "Template " ++ show mi ++ " must be instantiated before inner types can be used!"
        f (Single mi v) = symbolError $ show i ++ " has no types to resolve!"
        f (Group _ vs) = evalMember <=< groupMemberFind i . fmap InMetaValue $ vs

evalMember :: FMetaValue -> Symbols FMetaValue
evalMember = outMetaValue >>> F.unfix >>> f
  where f :: MetaValue (F.Fix MetaValue) -> Symbols FMetaValue
        f (Single _ v) = evalExpr (outExpr v)
        f (Group mi vs) = (inFMetaValue . Group mi . fmap outMetaValue)
                      <$> (sequence . fmap (evalMember . InMetaValue) $ vs)
        f (Template mi ss) = return . inFMetaValue $ Template mi ss

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
        f (Type t) = pure (inFMetaValue (Single (Id "") (inFExpr $ Type t)))
