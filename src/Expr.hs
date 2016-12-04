module Expr(
  Expr(..),
  FExpr(..),
  (.:),
  (<:>),
  groupMemberFind,
  evalExpr,
  evalExpr'
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
import MathExpr
import BooleanExpr

outFMetaValue = F.unfix . outMetaValue

outFExpr = F.unfix . outExpr

wrapValue mi = inFMetaValue . Single mi . inFExpr . Type . Right

applyBinaryExpr :: Id
                -> (Value -> Value -> Either String Value)
                -> FMetaValue
                -> FMetaValue
                -> Symbols FMetaValue
applyBinaryExpr mi f x y = case (outFMetaValue x, outFMetaValue y) of
  (Single _ x', Single _ y') -> case (outFExpr x', outFExpr y') of
    (Type (Right x''), Type (Right y'')) -> Lookup . const . fmap (wrapValue mi) $ f x'' y''
    _ -> symbolError "Cannot apply binary operation to non-values"
  _ -> symbolError "Cannot apply binary operation to non-values"

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
        f (Single mi v) = evalExpr mi (outExpr v)
        f (Group mi vs) = (inFMetaValue . Group mi . fmap outMetaValue)
                      <$> (sequence . fmap (evalMember . InMetaValue) $ vs)
        f (Template mi ss) = return . inFMetaValue $ Template mi ss

evalArgs tbl = join
             . fmap (sequence . fmap getValue)
             . sequence
             . fmap (evalSymbols tbl)

evalExpr' = evalExpr (Id "")

evalBinaryOp mi f x y = join $ pure (applyBinaryExpr mi f) <*> x <*> y

evalExpr :: Id -> F.Fix Expr -> Symbols FMetaValue
evalExpr mi = F.cata f
  where f :: Expr (Symbols FMetaValue) -> Symbols FMetaValue
        -- Scope resolution
        f (Scope e i) =  e >>= evalScope i

        -- Template instantiation
        f (Instantiate e args) = (Lookup $ \tbl -> do
          e' <- evalSymbols tbl e
          args' <- evalArgs tbl args
          instantiate e' args') >>= evalMember

        -- Math Operations
        f (ADD x y) = evalBinaryOp mi addition x y
        f (MUL x y) = evalBinaryOp mi multiplication x y
        f (SUB x y) = evalBinaryOp mi subtraction x y
        f (DIV x y) = evalBinaryOp mi division x y

        -- A single type
        f (Type (Right (USR t))) = lookupId t

        -- Templated Type
        f (Type (Right (USRT i ts))) = do
          i' <- lookupId i
          (Lookup $ \tbl -> do
            ts' <- evalArgs tbl . fmap (evalExpr i) $ ts
            instantiate i' ts')

        f (Type t) = pure (inFMetaValue (Single mi (inFExpr $ Type t)))
