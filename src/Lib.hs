{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Free

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Id = Id String deriving (Eq, Ord, Show)

data Value a = INT
            | FLOAT
            | DOUBLE
            | CHAR
            | UINT
            | SHORT
            | LONG
            | VOID
            | BOOL
            | CONST a
            | PTR a
            | REF a
            | STATIC a
            | USR Id
              deriving Functor

--   Fixed Type
data FValue = InValue { outValue :: F.Fix Value }

instance Show FValue where
  show = outValue >>> F.cata f
         where f (INT) = "int"
               f (FLOAT) = "float"
               f (DOUBLE) = "double"
               f (CHAR) = "char"
               f (UINT) = "uint"
               f (SHORT) = "short"
               f (LONG) = "long"
               f (VOID) = "void"
               f (BOOL) = "bool"
               f (CONST a) = "const " ++ a
               f (PTR a) = a ++ "*"
               f (STATIC a) = "static " ++ a
               f (USR t) = t

data MetaArg = Targ Id
              | Tlist Id

instance Show MetaArg where
  show (Targ t) = "class " ++ t
  show (Tlist t) = "class ..." ++ t

data MetaFunc a = MetaFunc ([FValue] -> Either String a) -- ^ Function

data MetaValue a = Template [MetaArg] (MetaFunc a)
                 | Group (Free (Record a)) -- ^ A struct or record
                 | Single FValue -- ^ static const int value = 5;

data Record a next = Record (Id,a) next
                   | EndRecord
                     deriving (Functor)

metaId (F.Fix (Template i _)) = i
metaId (F.Fix (Group i _)) = i
metaId (F.Fix (Alias i _)) = i
metaId (F.Fix (Single i _)) = i

newtype FMetaValue = InMetaValue { outMetaValue :: F.Fix MetaValue }

showCommaList = concat . intersperse "," . map show

instance Show FMetaValue where
  show = outMetaValue >> F.cata f
         where f (Single v) = show v
               f (Group vs) = concatMap (flip (++) "\n" . show) $ vs
               f (Template args f) = "template<" ++ showCommaList args ++ ">"

example = Template [Targ "T"] $ \args ->
  case args of
  [t] -> Group 

data Expr a = Scope a Id -- someClass::value
            | Instantiate Id [FValue] -- add<1,2>
            | Type FValue


--   Fixed Expression
data FExpr = InExpr { outExpr::F.Fix Expr }

data Stmt a = Using Id FExpr

type SymbolTable = Map.Map Id FMetaValue

{-
   In order to make working with the symbol table easier
   we are going to define two types with certain properties
   that will allow us to cleanly define actions using the
   table.
-}

data Symbols t = Definition (SymbolTable -> Either String (t, SymbolTable))
               | Lookup (SymbolTable -> Either String t)

instance Functor Symbols where
  fmap = liftM

{-
    Functors are parametric types which can be mapped over

    fmap :: Functor f => (a -> b) -> (f a -> f b)
-}

instance Applicative Symbols where
  pure a = Lookup . const $ Right a
  (<*>) = ap

{-
    Applicative types allow for function application within the
    context of functions. Applicative types must have definitions
    for the function "pure" which can wrap any value into a functor
    as well as <*> which is pronounced "apply".

    pure :: Functor f => a -> f a

    <*> :: Functor f => (a -> b) -> f a -> f b
-}

instance Monad Symbols where

  Definition ma >>= f = Definition $ ma >=> \(a, tbl') ->
    case f a of
      Definition df -> df tbl'
      Lookup lf -> fmap (flip (,) tbl') (lf tbl')

  Lookup ma >>= f = Definition $ \tbl ->
    case fmap f (ma tbl) of
      Right (Definition df) -> df tbl
      Right (Lookup lf) -> fmap (flip (,) tbl) (lf tbl)
      Left e -> Left e

{-
   Monads allow for composition of functions which return Functors.
   Composition with functors can be achieved with

   >=> :: Applicative f => (a -> f b) -> (b -> f c) -> (a -> f c)

   However it suffices to define a function called >>= pronounced
   "bind" or "shove". This function defines how to extract the inner
   value from a functor and pass it to the next function.

   >>= :: m a -> (a -> m b) -> m b

   The shove function has a variant >> which will ignore the result of
   the previous function and simply execute the next one with the context
   of the last Functor.

   >> :: m a -> m b -> m b
   ma >> mf = ma >>= (\a -> mf)

   The two functions >>= and >> can be used to implement the two imperative
   statement actions variable declaration and function calls.

   Here is an example with the IO Monad and how it might be written in C++

   main = (print "Enter input1" >>
           getLine >>= (\input1 ->
           print "Enter input2" >>
           getLine >>= (\input2 ->
           print ("You entered " ++ input1 ++ " " ++ input2))))

   int main(){

      std::cout << "Enter Input1\n";
      std::cin >> input1;

      std::cout << "Enter Input2\n";
      std::cin >> input2;

      std::cout << "You entered " << input1 << " " << input2;
   }

   The monad notation can be short handed like so...

   main = do
     print "Enter input1"
     input1 <- getLine
     print "Enter input2"
     input2 <- getLine
     print "You entered " ++ input1 ++ " " ++ input2

   Assignment is done with <- which signifies that the result of the monadic
   function on the right is being bound to the symbol on the left. Functions
   called without the <- are functions on the functor that is being passed
   through the sequence.

-}

lookup :: Id -> Symbols (Maybe FMetaValue)
-- ^ Gets the type for an id
lookup i = Lookup $ pure . Map.lookup i

define :: F.Fix MetaValue -> Symbols ()
-- ^ Addes a symbol value pair to the symbol table
define mv = Definition $ \tbl ->
  if isNothing (Map.lookup (metaId mv) tbl)
    then Right ((), Map.insert (metaId mv) (InMetaValue mv) tbl)
    else Left $ "Unknown symbol " ++ (show . metaId $ mv)

--apply :: FExpr -> Symbols (Maybe FValue)
-- ^ Looks up the metavalue and attempts to apply it to the args
--apply (InExpr (Instantiate id args)) = fmap (<*> pure args) (Lib.lookup id)
--apply _ = Error "Can only apply an instantiate expression!"

-- type Fix f = f (Fix f)
--
-- project (Fix a) = a
--
-- cata :: (Type a -> a) -> Fix Type -> a
-- cata f = c where c = f . fmap c . project


-- example = do
--   templateClass [Class "T"] "test" $ do
--     using [] "value" (Id "T")
--     using [] "result" (Const (Id "T"))
