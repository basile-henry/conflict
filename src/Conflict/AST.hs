{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Conflict.AST
  ( -- * AST
    Program (..)
  , Statement (..)
  , Expr (..)
  , LExpr (..)
  , Term (..)
  , Factor (..)
  , Var (..)
  , Label (..)
  , Literal (..)

    -- * Literal helper functions
  , literalToBool
  , literalToInt
  , literalToString
  , isBool
  , isInt
  , isString
  ) where

-- base
import           Data.Bool  (bool)
import           Data.Maybe (fromMaybe)

-- text
import           Data.Text

-- | A full @Conflict@ program made up of a many 'Statement's
newtype Program = Program [Statement] deriving (Show, Eq)

-- | A single statement, corresponds a line in the 'Program' with the exception
--   of the 'Conflict' statement which always spans at least 3 lines (when both
--   sides of the conflict are empty).
data Statement
  = Conflict [Statement] [Statement]
  -- ^ A merge conflict (git style)
  --
  --   @
  --   <<<<<<<< Some description
  --   a = b + c
  --   ========
  --   goto start
  --   >>>>>>>> Some other description
  --   @
  --
  --   Both sides of a conflict are executed concurrently, upon reaching the
  --   end of the statements, a thread waits for the other one to finish its
  --   side before only one continues.
  --
  --   Nested conflicts are permitted, interleaved conflicts are not and lead to
  --   a syntax error.
  --
  --   @goto@ exiting a conflict block may lead to a "fork bomb".
  | Assign Var Expr
  -- ^ Variable assignment.
  | Anchor Label
  -- ^ A label anchor to go to.
  --
  --   @
  --   [start]
  --   @
  --
  --   Multiple anchors with the same label have undefined behaviour, either one
  --   could be used.
  | GoTo Label
  -- ^ An unconditional go to.
  | Branch Expr Label
  -- ^ A conditional go to (Go to the label if the condition is /truthy/).
  | Print Expr
  -- ^ Print the result of an expression and a newline to STDOUT.
  | Input Var
  -- ^ Read a line from STDIN.
  deriving (Show, Eq)

-- | An expression to compute a value.
data Expr
  = GT LExpr LExpr
  | LT LExpr LExpr
  | GE LExpr LExpr
  | LE LExpr LExpr
  | NE LExpr LExpr
  | EQ LExpr LExpr
  | Expr LExpr
  deriving (Show, Eq)

-- | An expression to compute a value.
data LExpr
  = Plus Term LExpr
  -- ^ Add two expressions:
  --
  --   - Addition on two Integers
  --   - Concatanation on two Strings
  --   - Cast both to Integers and do addition otherwise
  --
  -- Results in an Integer.
  | Sub Term LExpr
  -- ^ Subtract two expressions:
  --
  --   Cast both to Integers and do subtraction
  --
  --   Results in an Integer.
  | Or Term LExpr
  -- ^ Logical /or/ between two expressions:
  --
  --   Cast both to Bools and do subtraction
  --
  --   Results in a Bool.
  | LExpr Term
  deriving (Show, Eq)

data Term
  = Mul Factor Term
  | Div Factor Term
  | Mod Factor Term
  | And Factor Term
  | Term Factor
  deriving (Show, Eq)

data Factor
  = Lit Literal
  -- ^ A literal.
  | Variable Var
  -- ^ A variable (previously defined or not).
  | Parens LExpr
  -- ^ An expression group.
  | Not LExpr
  -- ^ Logical negation.
  deriving (Show, Eq)

-- | A variable holding a value (string, boolean or integer)
--
--   Variables have a default value (when used while undefined) which
--   corresponds to the /falsy/ values.
--
--   - Integers: @0@
--   - Strings: empty string
--   - Bool: False
--
--   Be careful, the type is inferred from the first use site of a variable.
--
--   Here, if @a@ is undefined:
--
--   @
--   b = a + 2
--   print "$a"
--   @
--
--   It will print @"0\n"@, whereas if the @b = a + 2@ wasn't there it would
--   infer the type of @a@ to be a String and print @"\n"@
data Var
  = Var Text
  | DictVar Text Expr
  deriving (Show, Eq)

-- | A label used to match anchors and GoTos (both conditional and
--   unconditional).
newtype Label = Label Text deriving (Show, Eq, Ord)

-- | Literals for boolean, strings, and integers
--
--   Values are weakly typed, everything has a /thruthy/ value:
--
--   - Strings: Empty string is @False@, the rest if @True@
--   - Integers: @0@ is @False@, the rest is @True@
--   - Bool: False is False, True is True
--
data Literal
  = BoolLit Bool
  -- ^ Boolean literals are one of @True@ or @False@.
  | IntLit Integer
  -- ^ Integers are unbounded.
  | StringLit Text
  -- ^ String literals are surrounded by double quotes.
  --   The following escaped characters are supported with their usual meaning:
  --
  --   @
  --   ['\n', '\r', '\t', '\"', \'\\\\']
  --   @
  --
  --   Strings support Unicode.
  deriving (Show, Eq)

------------------------------
-- Literal helper functions --
------------------------------

literalToBool :: Literal -> Bool
literalToBool = \case
  BoolLit b   -> b
  IntLit i    -> i /= 0
  StringLit s -> s /= ""

literalToInt :: Literal -> Integer
literalToInt = \case
  BoolLit b   -> bool 0 1 b
  IntLit i    -> i
  StringLit s -> fromMaybe 0 $ readMaybe s
  where
    readMaybe s =
      case reads $ unpack s of
        [(x, "")] -> Just x
        _         -> Nothing

literalToString :: Literal -> Text
literalToString = \case
  BoolLit b   -> pack $ show b
  IntLit i    -> pack $ show i
  StringLit s -> s

isBool :: Literal -> Bool
isBool = \case
  BoolLit _ -> True
  _         -> False

isInt :: Literal -> Bool
isInt = \case
  IntLit _ -> True
  _        -> False

isString :: Literal -> Bool
isString = \case
  StringLit _ -> True
  _           -> False
