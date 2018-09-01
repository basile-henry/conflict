module Conflict.AST where

import           Data.Text

-- | A full @Conflict@ program made up of a many 'Statement's
newtype Program = Program [Statement]

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
  --   Multiple anchors with the same label have undefined behaviour, either one
  --   could be used.
  | GoTo Label
  -- ^ An unconditional go to.
  | Branch Expr Label
  -- ^ A conditional go to (Go to the label if the condition is True).
  | Print Expr
  -- ^ Print the result of an expression and a newline to STDOUT.
  | Input
  -- ^ Read a line from STDIN.

-- | An expression to compute a value.
data Expr
  = Lit Literal
  -- ^ A literal
  | VarExpr Var
  -- ^ A variable (previously defined or not)

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
newtype Var = Var Text

-- | A label used to match anchors and GoTos (both conditional and
--   unconditional).
newtype Label = Label Text

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
  --   Expressions can be spliced into strings literals using a @${ ... }@
  --   block, where @...@ is the expression.
  --   The following escaped characters are supported with their usual meaning:
  --
  --   @
  --   ['\n', '\r', '\t', '\"', \'\\$', \'\\\\']
  --   @
  --
  --   Strings support Unicode.
