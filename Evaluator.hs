module Evaluator where

import Control.Monad.Error

import Cantor

-- Evaluation
eval :: Env -> Expr -> ThrowsError Expr
eval env val@(Number _) = return val
eval env val@(Boolean _) = return val
eval env (Id id) = do envId <- getId env id
                      case envId of
                          LazyExpr e expr -> eval e expr
                          otherwise -> return envId
eval env (Lambda id expr) = return $
    Function (\e a -> eval e a >>= \arg -> eval (bindId env id arg) expr)
eval env (Lazy id expr) = return $
    Function (\e a -> eval (bindId env id (LazyExpr e a)) expr)
eval env (Fix id expr) =
    eval (bindId env id (LazyExpr env $ Fix id expr)) expr
eval env (Let id e1 e2) = do
    eval env e1 >>= \lt -> eval (bindId env id lt) e2
eval env (Apply fn arg) = do
    func <- eval env fn
    case func of
        (Function f) -> f env arg
        otherwise -> throwError $ Evaluator $ "Application of non-function "++(show func)

-- Environment
getId :: Env -> String -> ThrowsError Expr
getId env id = maybe (throwError $ UnboundVariable id) (return) (lookup id env)

bindId :: Env -> String -> Expr -> Env
bindId env id expr = (id, expr):env

