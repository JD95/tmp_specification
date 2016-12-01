module Symbols (
  SymbolTable,
  Symbols(..),
  symbolError,
  execSymbols,
  evalSymbols,
  lookupId,
  define
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

type SymbolTable = Map.Map Id MetaValue

data Symbols t = Definition (SymbolTable -> Either String (t, SymbolTable))
               | Lookup (SymbolTable -> Either String t)

symbolError = Lookup . const . Left

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

execSymbols :: Symbols a -> SymbolTable -> Either String (a, SymbolTable)
execSymbols (Definition mf) tbl = mf tbl
execSymbols (Lookup mf) tbl = flip (,) tbl <$> mf tbl

evalSymbols :: SymbolTable -> Symbols a -> Either String a
evalSymbols tbl (Definition mf)  = fmap fst (mf tbl)
evalSymbols tbl (Lookup mf) = mf tbl


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

lookupId :: Id -> Symbols MetaValue
-- ^ Gets the type for an id
lookupId i = Lookup $ \tbl -> case Map.lookup i tbl of
  Just mv -> Right mv
  Nothing -> Left $ "Symbol " ++ show i ++ " is undefined!"

define :: MetaValue -> Symbols ()
-- ^ Addes a symbol value pair to the symbol table
define mv = Definition $ \tbl ->
  if isNothing (Map.lookup (metaId mv) tbl)
    then Right ((), Map.insert (metaId mv) mv tbl)
    else Left $ "Unknown symbol " ++ (show . metaId $ mv)
