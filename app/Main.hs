module Main where

-- base
import           System.Environment

-- text
import qualified Data.Text.IO       as Text

-- conflict
import           Conflict.Interpret (interpret)
import           Conflict.Parser    (parser)

main :: IO ()
main = do
  let useAst file f = do
        input <- Text.readFile file
        case parser "stdin" input of
          Right ast -> f ast
          Left  err -> putStrLn err
  args <- getArgs
  case args of
    ["--ast", file ] -> useAst file print
    [ file ]         -> useAst file interpret
    _                -> print $ "Unexpected arguments: " ++ show args
