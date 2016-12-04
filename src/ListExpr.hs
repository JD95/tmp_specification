module ListExpr (

) where

import qualified Data.Functor.Foldable as F
import Control.Monad

import MetaValue
import Symbols
import Value

-- packTail :: Value -> Either String Value
-- packTail (PACK ([t])) = Right t
-- packTail (PACK (t:ts)) = Right (PACK ts)
-- packTail _ = Left $ "Cannot perform tail"
