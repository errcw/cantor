module Library (typeEnv, evalEnv) where

import Control.Monad.Error
import qualified Data.Map as Map

import Cantor
import Evaluator

typeEnv :: TypeEnv
typeEnv = TypeEnv $ Map.fromList (zip ids schemes)
    where (ids, _, types) = unzip3 primitives
          schemes = map (\t -> Scheme ["a"] t) types

evalEnv :: Env
evalEnv = zip ids exprs
    where (ids, exprs, _) = unzip3 primitives

primitives :: [(String, Expr, Type)]
primitives = [
          ("+", binaryNumOp (+) Number, tBinaryNumOp),
          ("-", binaryNumOp (-) Number, tBinaryNumOp),
          ("*", binaryNumOp (*) Number, tBinaryNumOp),
          ("/", binaryNumOp div Number, tBinaryNumOp),
          ("/", binaryNumOp mod Number, tBinaryNumOp),
          ("~", unaryNumOp negate Number, tUnaryNumOp),
          ("=", binaryNumOp (==) Boolean, tBinaryBoolOp),
          ("<", binaryNumOp (<) Boolean, tBinaryBoolOp),
          ("<=", binaryNumOp (<=) Boolean, tBinaryBoolOp),
          (">", binaryNumOp (>) Boolean, tBinaryBoolOp),
          (">=", binaryNumOp (>=) Boolean, tBinaryBoolOp),
          ("if", ifOp, tIfOp)]

tUnaryNumOp = TFunction TNumber TNumber
tBinaryNumOp = TFunction TNumber tUnaryNumOp
tBinaryBoolOp = TFunction TNumber (TFunction TNumber TBoolean)
tIfOp = TFunction TBoolean (TFunction (TVar "a") (TFunction (TVar "a") (TVar "a")))

unaryNumOp :: (Integer -> a) -> (a -> Expr) -> Expr
unaryNumOp op expr = collectArgs 1 doOp
    where doOp args = do [arg] <- mapM evalArg args >>= mapM unpackNum
                         return $ expr $ op arg

binaryNumOp :: (Integer -> Integer -> a) -> (a -> Expr) -> Expr
binaryNumOp op expr = collectArgs 2 doOp
    where doOp args = do [left, right] <- mapM evalArg args >>= mapM unpackNum
                         return $ expr $ left `op` right

unpackNum :: Expr -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x = throwError $ Evaluator $ "Non-numeric argument for numeric function " ++ (show x)

ifOp :: Expr
ifOp = collectArgs 3 doIf
    where doIf [(ec, cond), (et, thn), (el, els)] = do
              cond <- eval ec cond
              case cond of
                  (Boolean b) -> if b
                                      then eval et thn
                                      else eval el els
                  otherwise -> throwError $ Evaluator $ "Non-boolean condition in if " ++ (show cond)

-- Helpers
collectArgs :: Int -> ([(Env, Expr)] -> ThrowsError Expr) -> Expr
collectArgs num func = mk num []
    where mk 1 args = Function (\e a -> func $ args ++ [(e, a)])
          mk n args = Function (\e a -> return $ mk (n - 1) (args ++ [(e, a)]))

evalArg :: (Env, Expr) -> ThrowsError Expr
evalArg (env, arg) = eval env arg

