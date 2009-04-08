module TypeInferencer where

import Cantor

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Types: free variables, applying subsitutions
class Types a where
    fv :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types Type where
    fv TNumber = Set.empty
    fv TBoolean = Set.empty
    fv (TVar n) = Set.singleton n
    fv (TFunction a r) = fv a `Set.union` fv r

    apply s (TVar n) = maybe (TVar n) id (Map.lookup n s)
    apply s (TFunction a r) = TFunction (apply s a) (apply s r)
    apply s t = t

instance Types TypeScheme where
    fv (Scheme vars t) = (fv t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types TypeEnv where
    fv (TypeEnv env) = fv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

instance Types a => Types [a] where
    fv l = foldr Set.union Set.empty (map fv l)
    apply s = map (apply s)

-- Substitutions
type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

-- Type environments: map from term variables to type schemes
remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = Scheme vars t
    where vars = Set.toList ((fv t) `Set.difference` (fv env))

-- Type states
data TIState = TIState { tiSupply :: Int }
type TI a = StateT TIState ThrowsError a

runTI :: TI a -> ThrowsError a
runTI t = do (res, _) <- runStateT t TIState{ tiSupply=0 }
             return res

freshVar :: String -> TI Type
freshVar prefix = do
    s <- get
    put s{tiSupply = tiSupply s + 1}
    return $ TVar $ prefix ++ show (tiSupply s)

instantiate :: TypeScheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> freshVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

-- Unification
mgu :: Type -> Type -> TI Subst
mgu TNumber TNumber = return nullSubst
mgu TBoolean TBoolean = return nullSubst
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TFunction l r) (TFunction l' r') = do s1 <- mgu l l'
                                           s2 <- mgu (apply s1 r) (apply s1 r')
                                           return $ s1 `composeSubst` s2
mgu t1 t2 = throwError $ TypeMismatch t1 t2

varBind :: String -> Type -> TI Subst
varBind u t
    | t == TVar u         = return nullSubst
    | u `Set.member` fv t = throwError $ TypeOccurs (TVar u) t
    | otherwise           = return $ Map.singleton u t

-- Type inference
ti :: TypeEnv -> Expr -> TI (Subst, Type)
ti _ (Number _) = return (nullSubst, TNumber)
ti _ (Boolean _) = return (nullSubst, TBoolean)
ti (TypeEnv env) (Id id) =
    case Map.lookup id env of
        Nothing -> throwError $ UnboundVariable id
        Just s  -> do t <- instantiate s
                      return (nullSubst, t)
ti env (Lambda id expr) = do
    tv <- freshVar "a"
    let TypeEnv env' = remove env id
        env'' = TypeEnv (env' `Map.union` (Map.singleton id (Scheme [] tv)))
    (s1, t1) <- ti env'' expr
    return (s1, TFunction (apply s1 tv) t1)
ti env (Lazy id expr) = ti env (Lambda id expr)
ti env (Fix id expr) = do
    tv <- freshVar "a"
    let TypeEnv env' = remove env id
        env'' = TypeEnv (env' `Map.union` (Map.singleton id (Scheme [] tv)))
    (s1, t1) <- ti env'' expr
    s2 <- mgu (apply s1 tv) t1
    return (s2 `composeSubst` s1, apply s2 t1)
ti env (Apply fn arg) = do
    tv <- freshVar "a"
    (s1, t1) <- ti env fn
    (s2, t2) <- ti (apply s1 env) arg
    s3 <- mgu (apply s2 t1) (TFunction t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (Let id e1 e2) = do
    (s1, t1) <- ti env e1
    let TypeEnv env' = remove env id
        t' = generalize (apply s1 env) t1
        env'' = TypeEnv (Map.insert id t' env')
    (s2, t2) <- ti (apply s1 env'') e2
    return (s1 `composeSubst` s2, t2)

inferType :: TypeEnv -> Expr -> TI Type
inferType env expr = do
    (s, t) <- ti env expr
    return (apply s t)

inftype :: TypeEnv -> Expr -> ThrowsError Type
inftype env expr = runTI (inferType env expr)

