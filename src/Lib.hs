module Lib
    ( someFunc
    ) where

import qualified Data.Functor.Foldable as F

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Id = Id String

data Type a = INT 
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
            | USR Id [(Id, Type a)] 

isInt :: Type Bool -> Bool
isInt terp = outType >>> cata f
             where f (INT) = True
                   f (CONST b) = b
                   f (PTR b) = b
                   f (REF b) = b
                   f (STATIC b) = b
                   _ -> False


--   Fixed Type
data FType = InType { outType :: F.Fix Type }

data MetaValue = T FType
               | SC (FType, String) -- FType MUST be STATIC (CONST a)
               | MetaFunction ([Ftype] -> Either String Ftype)

data Expr a = Instantiation Id [FType]
            | ScopeResolution a Id


--   Fixed Expression
data FExpr = InExpr { out::F.Fix Expr }

data Stmt a = Using { usingName :: String
                    , usingArgs :: [String]
                    , usingExpr :: ([String] -> Maybe FExpr)
                    }              

data Template = Template { tempName :: String
                         , tempArgs :: TArgs
                         , tempBody :: [(Id, MetaValue)]
                         } deriving (Show)

newtype SymbolTable = SymbolTable [(String, FType)]

{-
   In order to make working with the symbol table easier
   we are going to define two types with certain properties
   that will allow us to cleanly define actions using the
   table.
-}

newtype Scope t = Scope (SymbolTable -> t)

{-
   The first type will be the parameterized type Scope.
   This type will represent functions that need to use
   the symbol table in order to return a value.

   Note that these functions do not have the capability of
   changing the table.
-}

instance Functor Scope where
  fmap f ma = \tbl -> f (ma tbl)

{-
    Functors are parametric types which can be mapped over

    fmap :: Functor f => (a -> b) -> (f a -> f b)
-}

instance Applicative Scope where
  pure a = \tbl -> a
  mf <*> a = \tbl -> (fmap (mf tbl) a) tbl

{-
    Applicative types allow for function application within the
    context of functions. Applicative types must have definitions
    for the function "pure" which can wrap any value into a functor
    as well as <*> which is pronounced "apply".

    pure :: Functor f => a -> f a

    <*> :: Functor f => (a -> b) -> f a -> f b
-}

instance Monad Scope where
  ma >>= f = \tbl -> f (ma tbl) tbl

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

newtype Symbols t = Symbols (SymbolTable -> (t, SymbolTable))

instance Functor Symbols where
  fmap f ma = \tbl -> let (t, tbl') = ma tbl in (f t, tbl')

instance Applicative Symbols where
  pure a = \tbl -> (a, tbl)
  mf <*> a = \tbl -> let (f, tbl') = mf tbl in (fmap f a) tbl'

instance Monad Symbols where
  ma >>= f = \tbl -> let (t, tbl') = ma tbl in (f s) tbl'

table :: Scope SymbolTable
-- ^ A utility function to access the table in the Symbols Monad
table = \s -> (s,s)

lookup :: Id -> Scope (Maybe Ftype)
-- ^ Gets the type for an id
lookup id = table >>= fmap (find id)

apply :: FExpr -> Scope (Maybe FType)
-- ^ Looks up the metavalue and attempts to apply it to the args
apply (inExpr (Instance id args)) = fmap (<*> pure args) (lookup id)
apply _ = pure (Nothing)

type Fix f = f (Fix f)

project (Fix a) = a

cata :: (Type a -> a) -> Fix Type -> a
cata f = c where c = f . fmap c . project

example = do
  templateClass [Class "T"] "test" $ do
    using [] "value" (Id "T")
    using [] "result" (Const (Id "T))
