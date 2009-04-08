module TypeInferencer where

import Cantor

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Types: free variables, applying subsitutions
class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types Type where
    ftv TNumber = Set.empty
    ftv TBoolean = Set.empty
    ftv (TVar n) = Set.singleton n
    ftv (TFunction a r) = ftv a `Set.union` ftv r

    apply s (TVar n) = maybe (TVar n) id (Map.lookup n s)
    apply s (TFunction a r) = TFunction (apply s a) (apply s r)
    apply s t = t

instance Types TypeScheme where
    ftv (Scheme vars t) = (ftv t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

instance Types a => Types [a] where
    ftv l = foldr Set.union Set.empty (map ftv l)
    apply s = map (apply s)

-- Substitutions
type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

-- Type schemes
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = Scheme vars t
    where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

instantiate :: TypeScheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> freshVar) vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

-- Type environments: map from term variables to type schemes
remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

replaceVar :: TypeEnv -> String -> Type -> TypeEnv
replaceVar env id t = TypeEnv (env' `Map.union` r)
    where TypeEnv env' = remove env id
          r = Map.singleton id (Scheme [] t)

freshVar :: TI Type
freshVar = do
    s <- get
    put s{tiSupply = tiSupply s + 1}
    return $ TVar $ "a" ++ show (tiSupply s)

-- Type states
data TIState = TIState { tiSupply :: Int }
type TI a = StateT TIState ThrowsError a

runTI :: TI a -> ThrowsError a
runTI t = do (res, _) <- runStateT t TIState{ tiSupply=0 }
             return res

-- Type inference (Algorithm M)
mgu TNumber TNumber = return nullSubst
mgu TBoolean TBoolean = return nullSubst
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TFunction l r) (TFunction l' r') = do s1 <- mgu l l'
                                           s2 <- mgu (apply s1 r) (apply s1 r')
                                           return (s1 `composeSubst` s2)
mgu t1 t2 = throwError $ TypeMismatch t1 t2

varBind u t
    | t == TVar u          = return nullSubst
    | u `Set.member` ftv t = throwError $ TypeOccurs (TVar u) t
    | otherwise            = return $ Map.singleton u t

ti :: TypeEnv -> Type -> Expr -> TI Subst
ti _ cons (Number _) = mgu TNumber cons
ti _ cons (Boolean _) = mgu TBoolean cons
ti (TypeEnv env) cons (Id id) =
    case Map.lookup id env of
        Nothing -> throwError $ UnboundVariable id
        Just s  -> instantiate s >>= mgu cons
ti env cons (Lazy id expr) = ti env cons (Lambda id expr)
ti env cons (Lambda id expr) = do
    t1 <- freshVar
    t2 <- freshVar
    s1 <- mgu cons (TFunction t1 t2)
    let env' = replaceVar env id t1
    s2 <- ti (apply s1 env') (apply s1 t2) expr 
    return (s2 `composeSubst` s1)
ti env cons (Apply e1 e2) = do
    t <- freshVar
    s1 <- ti env (TFunction t cons) e1
    s2 <- ti (apply s1 env) (apply s1 t) e2
    return (s2 `composeSubst` s1)
ti env cons (Let id e1 e2) = do
    t <- freshVar
    s1 <- ti env t e1
    let TypeEnv env' = remove env id
        t' = generalize (apply s1 env) (apply s1 t)
        env'' = TypeEnv (Map.insert id t' env')
    s2 <- ti (apply s1 env'') (apply s1 cons) e2
    return (s2 `composeSubst` s1)
ti env cons (Fix id expr) = do
    ti (replaceVar env id cons) cons expr

inferType :: TypeEnv -> Expr -> TI Type
inferType env expr = do
    v <- freshVar
    s <- ti env v expr
    return (apply s v)

inftype :: TypeEnv -> Expr -> ThrowsError Type
inftype env expr = runTI (inferType env expr)

