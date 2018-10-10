{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Conflict.Interpret
  ( interpret
  ) where

-- base
import           Control.Concurrent          (forkIO)
import qualified Control.Concurrent.MVar     as MVar
import           Control.Monad               (void)
import qualified Data.Maybe                  as Maybe
import           Prelude                     hiding (Ordering (..))

-- containers
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Sequence               as Seq

-- monad-extras
import           Control.Monad.Loops         (unfoldrM)

-- stm
import           Control.Concurrent.STM      (STM)
import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar

-- text
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text

-- conflict
import           Conflict.AST

-- | Map of all the vars used/defined to far
newtype Vars = Vars (TVar (Map Text Literal))

interpret :: Program -> IO ()
interpret (Program statements) = do
  vars <- TVar.newTVarIO Map.empty
  interpretStatements (Vars vars) statements

---------------------
-- Value utilities --
---------------------

getKey :: Vars -> Var -> STM Text
getKey vars var =
  case var of
    Var x       -> pure x
    DictVar x e -> do
      y <- interpretExpr vars e
      pure $ Text.concat [x, "[", literalToString y, "]"]

setVar :: Vars -> Var -> Literal -> STM ()
setVar vars@(Vars vars') var lit = do
  key <- getKey vars var
  TVar.modifyTVar' vars' $ Map.insert key lit

getVar :: Vars -> Var -> STM Literal
getVar vars@(Vars vars') var = do
  key   <- getKey vars var
  varsM <- TVar.readTVar vars'
  pure . Maybe.fromMaybe (StringLit "") $ Map.lookup key varsM

---------------
-- Statement --
---------------

interpretStatements :: Vars -> [Statement] -> IO ()
interpretStatements vars statements = do
  let anchors
        = Map.fromList
        . Maybe.mapMaybe (\case
            (Anchor label, index) -> Just (label, index)
            _                     -> Nothing)
        $ zip statements [0..]

      statements' = interpretStatement vars <$> Seq.fromList statements

  void . flip unfoldrM 0 $ \i ->
    case Seq.lookup i statements' of
      Nothing       -> pure Nothing
      Just getLabel -> do
        label <- getLabel
        let programCounter =
              Maybe.fromMaybe i $ do
                l <- label
                Map.lookup l anchors

        -- Go to the next index or use the one after the label
        pure $ Just ((), succ programCounter)

interpretStatement :: Vars -> Statement -> IO (Maybe Label)
interpretStatement vars = \case
  Conflict a b -> do
    doneFlag <- MVar.newEmptyMVar

    forkIO $ do
      interpretStatements vars a
      MVar.putMVar doneFlag True

    interpretStatements vars b
    -- Block until other thread is done
    MVar.readMVar doneFlag

    pure Nothing

  Assign var expr -> STM.atomically $ do
    value <- interpretExpr vars expr
    setVar vars var value
    pure Nothing

  Anchor _ -> pure Nothing

  GoTo label -> pure $ Just label

  Branch expr label -> STM.atomically $ do
    value <- interpretExpr vars expr
    pure $
      if literalToBool value
      then Just label
      else Nothing

  Print expr -> do
    value <- STM.atomically $ interpretExpr vars expr
    Text.putStrLn $ literalToString value
    pure Nothing

  Input var -> do
    line <- Text.getLine
    STM.atomically $ setVar vars var $ StringLit line
    pure Nothing

----------------
-- Expression --
----------------

interpretExpr :: Vars -> Expr -> STM Literal
interpretExpr vars =
  let boolExpr f aExpr bExpr = do
        a <- interpretLExpr vars aExpr
        b <- interpretLExpr vars bExpr
        let toInt = IntLit . literalToInt
            res
              | sameType a b = f a b
              | otherwise    = f (toInt a) (toInt b)
        pure $ BoolLit res
  in  \case
    GT a b -> boolExpr (>)  a b
    LT a b -> boolExpr (<)  a b
    GE a b -> boolExpr (>=) a b
    LE a b -> boolExpr (<=) a b
    NE a b -> boolExpr (/=) a b
    EQ a b -> boolExpr (==) a b
    Expr e -> interpretLExpr vars e

-----------------
-- LExpression --
-----------------

interpretLExpr :: Vars -> LExpr -> STM Literal
interpretLExpr vars = \case
  Plus aLExpr bLExpr -> do
    a <- interpretTerm vars aLExpr
    b <- interpretLExpr vars bLExpr
    pure $
      if isString a || isString b
      then StringLit $ literalToString a <> literalToString b
      else IntLit $ literalToInt a + literalToInt b
  Sub aLExpr bLExpr -> do
    a <- interpretTerm vars aLExpr
    b <- interpretLExpr vars bLExpr
    pure $ IntLit $ literalToInt a - literalToInt b
  Or aLExpr bLExpr -> do
    a <- interpretTerm vars aLExpr
    b <- interpretLExpr vars bLExpr
    pure $ BoolLit $ literalToBool a || literalToBool b
  LExpr e -> interpretTerm vars e

----------
-- Term --
----------

interpretTerm :: Vars -> Term -> STM Literal
interpretTerm vars =
  let intTerm g f t = do
        a <- interpretFactor vars f
        b <- interpretTerm vars t
        pure . IntLit $ g (literalToInt a) (literalToInt b)
  in  \case
    Mul a b -> intTerm (*) a b
    Div a b -> intTerm div a b
    Mod a b -> intTerm mod a b
    And f t -> do
      a <- interpretFactor vars f
      b <- interpretTerm vars t
      pure . BoolLit $ literalToBool a && literalToBool b
    Term f -> interpretFactor vars f

------------
-- Factor --
------------

interpretFactor :: Vars -> Factor -> STM Literal
interpretFactor vars = \case
  Lit lit      -> pure lit
  Variable var -> getVar vars var
  Parens lExpr -> interpretLExpr vars lExpr
  Not lExpr    -> BoolLit . not . literalToBool <$> interpretLExpr vars lExpr
