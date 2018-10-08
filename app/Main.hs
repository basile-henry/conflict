module Main where

-- text
import qualified Data.Text.IO    as Text

-- conflict
import           Conflict.Parser (parser)

main :: IO ()
main = do
  input <- Text.getContents
  let result = parser "stdin" input
  case result of
    Right ast -> print ast
    Left  err -> putStrLn err

