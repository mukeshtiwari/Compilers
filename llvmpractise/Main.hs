module Main where

import Parser 
import Control.Monad.Trans


process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main = loop "repl>>" where 
   loop :: String  -> IO ()
   loop prompt = do
        putStr prompt 
        exp <- getLine 
        case exp of 
            ":x" -> putStrLn "Bye"
            _ ->  process exp  >> loop  prompt
      