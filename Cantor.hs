module Cantor where

import Control.Monad.Error
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (ParseError)

-- Abstract syntax
data Expr = Number Integer
          | Boolean Bool
          | Id String
          | Lambda String Expr
          | Lazy String Expr
          | Fix String Expr
          | Let String Expr Expr
          | Apply Expr Expr
          | Function (Env -> Expr -> ThrowsError Expr)
          | LazyExpr Env Expr

instance Show Expr where
    show (Number n) = show n
    show (Boolean b) = show b
    show (Id i) = i
    show (Lambda id expr) = "lambda "++id++"."++(show expr)
    show (Lazy id expr) = "lazy "++id++"."++(show expr)
    show (Fix id expr) = "rec "++id++"."++(show expr)
    show (Let id letE inE) = "let "++id++"="++(show letE)++" in "++(show inE)
    show (Apply fn arg) = "("++(show fn)++" "++(show arg)++ ")"
    show (Function _) = "<function>"
    show (LazyExpr _ expr) = "lazy<"++(show expr)++">"

-- Types
data Type = TNumber
          | TBoolean
          | TFunction Type Type
          | TVar String
          deriving (Eq, Ord)

data TypeScheme = Scheme [String] Type

newtype TypeEnv = TypeEnv (Map.Map String TypeScheme)

instance Show Type where
    show TNumber = "int"
    show TBoolean = "bool"
    show (TFunction from to) = (show from)++"->"++(show to)
    show (TVar var) = var

instance Show TypeScheme where
    show (Scheme vars t) = "all "++(unwords vars)++(show t)


-- Environment
type Env = [(String, Expr)]

-- Errors
data CantorError = Parser ParseError
                 | TypeMismatch Type Type
                 | TypeOccurs Type Type
                 | UnboundVariable String
                 | Evaluator String
                 | Default String

instance Error CantorError where
    noMsg = Default "A fatal error occurred"
    strMsg = Default

instance Show CantorError where
    show (Parser err) = "Parser error: " ++ (show err)
    show (TypeMismatch t1 t2) = "Type error: expected " ++ (show t1) ++ " but got " ++ (show t2)
    show (TypeOccurs t1 t2) = "Type error: " ++ (show t1) ++ " occurs in " ++ (show t2)
    show (UnboundVariable v) = "Unbound variable " ++ v
    show (Evaluator err) = "Evaluator error: " ++ err

type ThrowsError = Either CantorError

trapError action = catchError action (return . show)
extractValue (Right val) = val

