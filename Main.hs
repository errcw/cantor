module Main where 

import Monad
import System.IO
import System.Console.Readline (readline)

import Cantor
import Parser
import TypeInferencer
import Evaluator
import Library

main = repl

repl :: IO ()
repl = until_ (== "quit") readExpr evalPrintExpr

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
readLine = do ln <- readline ""
              case ln of
                  Nothing -> return ""
                  Just line -> return line
