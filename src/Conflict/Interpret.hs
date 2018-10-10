{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Conflict.Interpret
  ( interpret
  ) where

-- base
import           Control.Concurrent           (forkIO)
import qualified Control.Concurrent.MVar      as MVar
import           Control.Monad                (forever, void)
import qualified Data.Maybe                   as Maybe
import           Prelude                      hiding (Ordering (..), read)

-- containers
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Sequence                as Seq

-- monad-extras
import           Control.Monad.Loops          (unfoldrM)

-- stm
import           Control.Concurrent.STM       (STM)
import qualified Control.Concurrent.STM       as STM
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Control.Concurrent.STM.TVar  (TVar)
import qualified Control.Concurrent.STM.TVar  as TVar

-- text
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text

-- conflict
import           Conflict.AST

data Context =
  Context
    { vars  :: TVar (Map Text Literal)
    , read  :: TMVar Text
    , write :: TMVar Text
    }

interpret :: Program -> IO ()
interpret (Program statements) = do
  context <- STM.atomically $ do
    vars  <- TVar.newTVar Map.empty
    read  <- TMVar.newEmptyTMVar
    write <- TMVar.newEmptyTMVar
    pure Context{..}

  -- Reader thread
  forkIO . forever $ do
    line <- Text.getLine
    STM.atomically $ TMVar.putTMVar (read context) line

  -- Writer thread
  forkIO . forever $ do
    line <- STM.atomically $ TMVar.takeTMVar (write context)
    Text.putStrLn line

  interpretStatements context statements

---------------------
-- Value utilities --
---------------------

getKey :: Context -> Var -> STM Text
getKey context var =
  case var of
    Var x       -> pure x
    DictVar x e -> do
      y <- interpretExpr context e
      pure $ Text.concat [x, "[", literalToString y, "]"]

setVar :: Context -> Var -> Literal -> STM ()
setVar context var lit = do
  key <- getKey context var
  TVar.modifyTVar' (vars context) $ Map.insert key lit

getVar :: Context -> Var -> STM Literal
getVar context var = do
  key   <- getKey context var
  varsM <- TVar.readTVar $ vars context
  pure . Maybe.fromMaybe (StringLit "") $ Map.lookup key varsM

---------------
-- Statement --
---------------

interpretStatements :: Context -> [Statement] -> IO ()
interpretStatements context statements = do
  let anchors
        = Map.fromList
        . Maybe.mapMaybe (\case
            (Anchor label, index) -> Just (label, index)
            _                     -> Nothing)
        $ zip statements [0..]

      statements' = interpretStatement context <$> Seq.fromList statements

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

interpretStatement :: Context -> Statement -> IO (Maybe Label)
interpretStatement context = \case
  Conflict a b -> do
    doneFlag <- MVar.newEmptyMVar

    forkIO $ do
      interpretStatements context a
      MVar.putMVar doneFlag True

    interpretStatements context b
    -- Block until other thread is done
    MVar.readMVar doneFlag

    pure Nothing

  Assign var expr -> STM.atomically $ do
    value <- interpretExpr context expr
    setVar context var value
    pure Nothing

  Anchor _ -> pure Nothing

  GoTo label -> pure $ Just label

  Branch expr label -> STM.atomically $ do
    value <- interpretExpr context expr
    pure $
      if literalToBool value
      then Just label
      else Nothing

  Print expr -> STM.atomically $ do
    value <- interpretExpr context expr
    TMVar.putTMVar (write context) $ literalToString value
    pure Nothing

  Input var -> STM.atomically $ do
    line <- TMVar.takeTMVar (read context)
    setVar context var $ StringLit line
    pure Nothing

----------------
-- Expression --
----------------

interpretExpr :: Context -> Expr -> STM Literal
interpretExpr context =
  let boolExpr f aExpr bExpr = do
        a <- interpretLExpr context aExpr
        b <- interpretLExpr context bExpr
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
    Expr e -> interpretLExpr context e

-----------------
-- LExpression --
-----------------

interpretLExpr :: Context -> LExpr -> STM Literal
interpretLExpr context = \case
  Plus aLExpr bLExpr -> do
    a <- interpretTerm context aLExpr
    b <- interpretLExpr context bLExpr
    pure $
      if isString a || isString b
      then StringLit $ literalToString a <> literalToString b
      else IntLit $ literalToInt a + literalToInt b
  Sub aLExpr bLExpr -> do
    a <- interpretTerm context aLExpr
    b <- interpretLExpr context bLExpr
    pure $ IntLit $ literalToInt a - literalToInt b
  Or aLExpr bLExpr -> do
    a <- interpretTerm context aLExpr
    b <- interpretLExpr context bLExpr
    pure $ BoolLit $ literalToBool a || literalToBool b
  LExpr e -> interpretTerm context e

----------
-- Term --
----------

interpretTerm :: Context -> Term -> STM Literal
interpretTerm context =
  let intTerm g f t = do
        a <- interpretFactor context f
        b <- interpretTerm context t
        pure . IntLit $ g (literalToInt a) (literalToInt b)
  in  \case
    Mul a b -> intTerm (*) a b
    Div a b -> intTerm div a b
    Mod a b -> intTerm mod a b
    And f t -> do
      a <- interpretFactor context f
      b <- interpretTerm context t
      pure . BoolLit $ literalToBool a && literalToBool b
    Term f -> interpretFactor context f

------------
-- Factor --
------------

interpretFactor :: Context -> Factor -> STM Literal
interpretFactor context = \case
  Lit lit      -> pure lit
  Variable var -> getVar context var
  Parens lExpr -> interpretLExpr context lExpr
  Not lExpr    -> BoolLit . not . literalToBool <$> interpretLExpr context lExpr
