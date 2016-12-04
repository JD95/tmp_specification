module MathExpr (
  addition,
  (+:),
  multiplication,
  (*:),
  subtraction,
  (-:),
  division,
  (/:)
) where

import qualified Data.Functor.Foldable as F
import Control.Monad

import MetaValue
import Symbols
import Value


addition :: Value -> Value-> Either String Value
addition (IntLit x) (IntLit y) = Right $ IntLit (x+y)
addition x y = Left $ "Cannot add " ++ show x ++ " and " ++ show y

add_ x y = F.Fix $ ADD x y
(+:) = add_

multiplication :: Value -> Value-> Either String Value
multiplication (IntLit x) (IntLit y) = Right $ IntLit (x * y)
multiplication x y = Left $ "Cannot multiply " ++ show x ++ " and " ++ show y

mul_ x y = F.Fix $ MUL x y
(*:) = mul_

subtraction :: Value -> Value -> Either String Value
subtraction x y = join $ pure (addition x) <*> multiplication (IntLit (-1)) y

sub_ x y = F.Fix $ SUB x y
(-:) = sub_

division :: Value -> Value -> Either String Value
division x (IntLit 0) = Left "Cannot divide by zero!"
division (IntLit x) (IntLit y) = Right $ IntLit (x `div` y)

div_ x y = F.Fix $ DIV x y
(/:) = div_
