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
program
  =   Program
  <$> many (many endOfLine *> statement <* many endOfLine)
  <*  eof

whitespace :: Parser ()
whitespace = void $ many (char ' ' <|> char '\t')

-- 'endOfLine' includes spaces and comments
endOfLine :: Parser ()
endOfLine = do
  whitespace
  comment <|> void newline
  where
    comment = try $ do
      void $ chunk "//"
      void $ many (anySingleBut '\n')
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
    conflictLine str = do
      void $ chunk str
      void $ skipManyTill (anySingleBut '\n') newline

assign :: Parser Statement
assign = do
  v <- var
  whitespace
  void $ single '='
  whitespace
  e <- expr
  pure $ Assign v e

anchor :: Parser Statement
anchor = Anchor <$> between (single '[') (single ']') label

goto :: Parser Statement
goto = do
  void $ chunk "goto"
  whitespace
  l <- label
  pure $ GoTo l

branch :: Parser Statement
branch = do
  void $ chunk "if"
  whitespace
  e <- expr
  whitespace
  l <- label
  pure $ Branch e l

print :: Parser Statement
print = do
  void $ chunk "print"
  whitespace
  e <- expr
  pure $ Print e

input :: Parser Statement
input = do
  void $ chunk "input"
  whitespace
  v <- var
  pure $ Input v

--------------------------------------------------
-- Expr
--------------------------------------------------

expr :: Parser Expr
expr = do
  a <- lExpr
  let helper cons sep = try $ do
        whitespace
        void $ chunk sep
        whitespace
        cons a <$> lExpr
  choice
    [ helper GT ">"
    , helper LT "<"
    , helper GE "<="
    , helper LE "<="
    , helper NE "!="
    , helper EQ "=="
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
        void $ chunk sep
        whitespace
        cons a <$> lExpr
  choice
    [ helper Plus "+"
    , helper Sub  "-"
    , helper Or   "||"
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
        void $ chunk sep
        whitespace
        cons a <$> term
  choice
    [ helper Mul "*"
    , helper Div "/"
    , helper Mod "%"
    , helper And "&&"
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
    parens = Parens <$>
      between
        (single '(' *> whitespace)
        (whitespace <* single ')')
        lExpr

    not = Not <$ single '~' <* whitespace <*> lExpr

--------------------------------------------------
-- Var
--------------------------------------------------

var :: Parser Var
var = do
  c  <- lowerChar
  cs <- many letterChar
  let name = pack (c : cs)
  choice
    [ try $ DictVar name <$> between (single '[') (single ']') expr
    , pure $ Var name
    ]

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
stringLit =
  StringLit . pack <$> between (single '"') (single '"') (many stringChar)
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
