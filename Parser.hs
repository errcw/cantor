module Parser (parseExpr) where

import Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P

import Cantor

parseExpr :: String -> ThrowsError Expr
parseExpr input =
    case parse program "cantor" input of
         Left err -> throwError $ Parser err
         Right expr -> return expr

-- Parser
program :: Parser Expr
program = do
    e <- expr
    eof
    return e

expr = lambda
   <|> lazy
   <|> fixe
   <|> lete
   <|> apply
   <|> ident
   <|> number

lambda = do
    reserved "lambda"
    id <- identifier; symbol "."
    e <- expr
    return (Lambda id e)

lazy = do
    reserved "lazy"
    id <- identifier; symbol "."
    e <- expr
    return (Lazy id e)

fixe = do
    reserved "fix"
    id <- identifier; symbol "."
    e <- expr
    return (Fix id e)

lete = do
    reserved "let"
    id <- identifier; symbol "="
    idE <- expr
    reserved "in"
    inE <- expr
    reserved "end"
    return (Let id idE inE)

apply = do
    symbol "("
    fn <- expr
    args <- sepEndBy1 expr spaces
    symbol ")"
    return $ foldl1 (\l r -> (Apply l r)) (fn:args)

ident = liftM Id $ (identifier <|> operator)

number = liftM Number $ natural

-- Lexer
lexer = P.makeTokenParser cantorDef

reserved = P.reserved lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
operator = P.operator lexer
natural = P.natural lexer

cantorDef
    = emptyDef
    { P.identStart = letter
    , P.identLetter = alphaNum
    , P.reservedNames = ["lambda", "lazy", "fix", "let", "in", "end"]
    , P.opStart = P.opLetter cantorDef
    , P.opLetter = oneOf "+-*/%<>=~"
    }

