{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Conflict.Parser
  ( parser
  ) where

-- base
import           Control.Monad        (void)
import           Data.Bifunctor       (first)
import           Data.Void            (Void)
import           Prelude              hiding (Ordering (..), not, print)

-- text
import           Data.Text            (Text, pack)

-- megaparsec
import           Text.Megaparsec      hiding (Label, label)
import           Text.Megaparsec.Char

-- conflict
import           Conflict.AST

type Parser = Parsec Void Text

parser :: String -> Text -> Either String Program
parser name
  = first errorBundlePretty
  . runParser program name

program :: Parser Program
program = Program <$> many statement <* eof

whitespace :: Parser ()
whitespace = void $ many (char ' ' <|> char '\t')

-- 'endOfLine' includes spaces and comments
endOfLine :: Parser ()
endOfLine = do
  whitespace
  comment <|> void newline
  where
    comment = try $ do
      chunk "//"
      many (anySingleBut '\n')
      void newline

--------------------------------------------------
-- Statement
--------------------------------------------------

statement :: Parser Statement
statement =
  choice $ map try
    [ conflict
    , input
    , anchor
    , goto
    , branch
    , print
    -- Check for assignment last
    , assign
    ]

conflict :: Parser Statement
conflict = do
  conflictLine "<<<<<<<"
  as <- many statement
  conflictLine "======="
  bs <- many statement
  conflictLine ">>>>>>>"
  pure $ Conflict as bs
  where
    conflictLine str =
      chunk str <* skipManyTill (anySingleBut '\n') newline

assign :: Parser Statement
assign = do
  v <- var
  whitespace
  single '='
  whitespace
  e <- expr
  whitespace
  optional endOfLine
  pure $ Assign v e


anchor :: Parser Statement
anchor = do
  l <- between (single '[') (single ']') label
  endOfLine
  pure $ Anchor l

goto :: Parser Statement
goto = do
  chunk "goto"
  whitespace
  l <- label
  endOfLine
  pure $ GoTo l

branch :: Parser Statement
branch = do
  chunk "if"
  whitespace
  e <- expr
  whitespace
  l <- label
  endOfLine
  pure $ Branch e l

print :: Parser Statement
print = do
  chunk "print"
  whitespace
  e <- expr
  endOfLine
  pure $ Print e

input :: Parser Statement
input = do
  chunk "input"
  whitespace
  v <- var
  endOfLine
  pure $ Input v

--------------------------------------------------
-- Expr
--------------------------------------------------

expr :: Parser Expr
expr = do
  a <- lExpr
  let helper cons sep = try $ do
        whitespace
        sep
        whitespace
        cons a <$> lExpr
  choice
    [ helper GT $ chunk ">"
    , helper LT $ chunk "<"
    , helper GE $ chunk "<="
    , helper LE $ chunk "<="
    , helper NE $ chunk "!="
    , helper EQ $ chunk "=="
    , pure $ Expr a
    ]

--------------------------------------------------
-- LExpr
--------------------------------------------------

lExpr :: Parser LExpr
lExpr = do
  a <- term
  let helper cons sep = try $ do
        whitespace
        sep
        whitespace
        cons a <$> lExpr
  choice
    [ helper Plus $ chunk "+"
    , helper Sub  $ chunk "-"
    , helper Or   $ chunk "||"
    , pure $ LExpr a
    ]

--------------------------------------------------
-- Term
--------------------------------------------------

term :: Parser Term
term = do
  a <- factor
  let helper cons sep = try $ do
        whitespace
        sep
        whitespace
        cons a <$> term
  choice
    [ helper Mul $ chunk "*"
    , helper Div $ chunk "/"
    , helper Mod $ chunk "%"
    , helper And $ chunk "&&"
    , pure $ Term a
    ]

--------------------------------------------------
-- Factor
--------------------------------------------------

factor :: Parser Factor
factor =
  choice
    [ Lit <$> literal
    , Variable <$> var
    , parens
    , not
    ]
  where
    parens = do
      single '('
      whitespace
      l <- lExpr
      whitespace
      single ')'
      pure $ Parens l

    not = Not <$ single '~' <* whitespace <*> lExpr

--------------------------------------------------
-- Var
--------------------------------------------------

var :: Parser Var
var = do
  c  <- lowerChar
  cs <- many letterChar
  pure $ Var $ pack (c : cs)

--------------------------------------------------
-- Label
--------------------------------------------------

label :: Parser Label
label = do
  c  <- lowerChar
  cs <- many letterChar
  pure $ Label $ pack (c : cs)

--------------------------------------------------
-- Literal
--------------------------------------------------

literal :: Parser Literal
literal =
  choice
    [ boolLit
    , intLit
    , stringLit
    ]

boolLit :: Parser Literal
boolLit =
  BoolLit <$>
    choice
      [ True <$ chunk "True"
      , False <$ chunk "False"
      ]

intLit :: Parser Literal
intLit = do
  m  <- optional (single '-')
  is <- some digitChar
  let x | Just i <- m = i:is
        | otherwise   = is
  pure $ IntLit $ read x

stringLit :: Parser Literal
stringLit = do
  single '"'
  cs <- many stringChar
  single '"'
  pure $ StringLit $ pack cs
  where
    escaped str c = c <$ chunk str

    stringChar =
      choice
        [ escaped "\\n" '\n'
        , escaped "\\r" '\r'
        , escaped "\\t" '\t'
        , escaped "\\\"" '"'
        , escaped "\\\\" '\\'
        , anySingleBut '"'
        ]
