module Main where 

import Monad
import System.Environment
import System.IO
import System.Console.Haskeline hiding (catch)

import Cantor
import Parser
import TypeInferencer
import Evaluator
import Library

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> repl
        1 -> repfile (args !! 0)
        otherwise -> putStrLn "Usage: Cantor [filename]"

repl :: IO ()
repl = until_ (== "quit") readExpr evalPrintExpr

repfile :: String -> IO ()
repfile name = do
    contents <- readFile name
    evalPrintExpr contents

readExpr :: IO String
readExpr = do
    line <- putStr "Cantor> " >> readLine
    if elem ';' line
        then return (takeWhile (/= ';') line)
        else do rest <- readExpr
                return (line ++ "\n" ++ rest)

evalPrintExpr :: String -> IO ()
evalPrintExpr str = putStrLn $ extractValue $ trapError $ evalString str

evalString :: String -> ThrowsError String
evalString str = do
    expr <- parseExpr str
    typed <- inftype typeEnv expr
    evaled <- eval evalEnv expr
    return $ (show evaled) ++ ":" ++ (show typed)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

readLine :: IO String
readLine = runInputT defaultSettings rd
    where rd = do ln <- getInputLine ""
                  case ln of
                      Nothing -> return ""
                      Just line -> return line

