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
import Data.List (nub, intercalate, find)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Id = Id String deriving (Eq, Ord)

instance Show Id where
  show (Id s) = s

data Value = INT
           | IntLit Int
           | FLOAT
           | DOUBLE
           | CHAR
           | UINT
           | SHORT
           | LONG
           | VOID
           | BOOL
           | BoolLit Bool
           | CONST Value
           | PTR Value
           | REF Value
           | STATIC Value
           | USR Id
             deriving (Eq, Ord)


instance Show Value where
  show INT = "int"
  show (IntLit a) = show a
  show FLOAT = "float"
  show DOUBLE = "double"
  show CHAR = "char"
  show UINT = "uint"
  show SHORT = "short"
  show LONG = "long"
  show VOID = "void"
  show BOOL = "bool"
  show (BoolLit a) = show a
  show (CONST a) = "const " ++ show a
  show (PTR a) = show a ++ "*"
  show (STATIC a) = "static " ++ show a
  show (USR t) = show t

data MetaArg = Targ Id
              | Tlist Id

-- Makes the definitions look nicer
class_ t = Targ (Id t)
list_ t = Tlist (Id t)

instance Show MetaArg where
  show (Targ t) = "class " ++ show t
  show (Tlist t) = "class ..." ++ show t

data MetaValue = Template Id [MetaArg] ([Value] -> Either String MetaValue)
                 | Group Id (Free (Line MetaValue) ()) -- ^ A struct or record
                 | Single Id Value -- ^ static const int value = 5;

data Line a next = Line a next
              | EndLine
                deriving (Functor)

listFromLines :: Line a [a] -> [a]
listFromLines (Line r rs) = r:rs
listFromLines EndLine = []

collapseGroupMembers = iter listFromLines . fmap (const [])

metaId (Template i _ _) = i
metaId (Group i _) = i
metaId (Single i _) = i

showCommaList = intercalate "," . map show

showGroupLines g@(Single i v) = [show g]
showGroupLines g@(Template _ args _) = [show g]
showGroupLines (Group i vs) = ["struct " ++ show i ++ "{"] ++ members ++ ["};"]
  where members = concatMap (fmap ("\t" ++) . showGroupLines) . collapseGroupMembers $ vs

instance Show MetaValue where
  show (Single i v) = show v ++ " " ++ show i
  show (Group i vs) = "struct " ++ show i ++ "{\n" ++ concat members ++ "};\n"
    where members = concatMap (fmap (\m -> "\t" ++ m ++ "\n") . showGroupLines) $ collapseGroupMembers vs
  show (Template i args _) = "template<" ++ showCommaList args ++ "> " ++ show i

single v i = Free (Line (Single (Id i) v) (Pure ()))

group i vs = Free (Line (Group (Id i) vs) (Pure ()))

template i args f = Free (Line (Template (Id i) args f) (Pure ()))

testClass = Group (Id "testClass") $ do
  single INT "x"
  template "add" [class_ "T1", class_ "T2"] $ \args -> case args of
      [IntLit x, IntLit y] -> Right $
        Group (Id "add") $ single (IntLit (x + y)) "value"
      _ -> Left "Add must have args of type int literal"
  group "testSubClass" $ do
    single DOUBLE "otherVar"

data Expr a = Scope a Id -- someClass::value
            | Instantiate Id [Value] -- add<1,2>
            | Type Value
              deriving (Functor)

--   Fixed Expression
data FExpr = InExpr { outExpr::F.Fix Expr }

scope_ e i = F.Fix $ Scope e (Id i)
(.:) = scope_

type_ t = F.Fix $ Type t

groupMemberFind :: Id -> [MetaValue] -> Either String MetaValue
groupMemberFind i ms = case find ((==) i. metaId) ms of
  Just m -> Right m
  Nothing -> Left $ show i ++ " undefined!"

evalExpr :: FExpr -> Symbols MetaValue
evalExpr = outExpr >>> F.cata f
  where f :: Expr (Symbols MetaValue) -> Symbols MetaValue
        f (Scope e i) = e >>= \expr -> case expr of
            Template mi _ _ -> Lookup . const . Left $ "Template " ++ show mi ++ " must be instantiated before inner types can be used!"
            Single mi _ -> Lookup . const . Left  $ show i ++ " has no types to resolve!"
            Group _ vs -> Lookup . const . groupMemberFind i . collapseGroupMembers $ vs
        f (Type (USR t)) = lookupId t
        --f (Type t) = Lookup . const .

data Stmt a = Using Id FExpr

type SymbolTable = Map.Map Id MetaValue

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

execSymbols :: Symbols a -> SymbolTable -> Either String (a, SymbolTable)
execSymbols (Definition mf) tbl = mf tbl
execSymbols (Lookup mf) tbl = flip (,) tbl <$> mf tbl

evalSymbols :: Symbols a -> SymbolTable -> Either String a
evalSymbols (Definition mf) tbl = fmap fst (mf tbl)
evalSymbols (Lookup mf) tbl = mf tbl


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


symbolsTest = do
  define testClass
  evalExpr (InExpr $ type_ (USR (Id "testClass")) .: "x")

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
