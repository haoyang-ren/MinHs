module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))

primOpType :: Op -> QType
primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = Ty $ Base Int `Arrow` Base Int
primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)

constType :: Id -> Maybe QType
constType "True"  = Just $ Ty $ Base Bool
constType "False" = Just $ Ty $ Base Bool
constType "()"    = Just $ Ty $ Base Unit
constType "Pair"  = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
constType "Inl"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType "Inr"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType _       = Nothing

type Gamma = E.Env QType

initialGamma :: Gamma
initialGamma = E.empty

tv :: Type -> [Id]
tv = tv'
 where
   tv' (TypeVar x) = [x]
   tv' (Prod  a b) = tv a `union` tv b
   tv' (Sum   a b) = tv a `union` tv b
   tv' (Arrow a b) = tv a `union` tv b
   tv' (Base c   ) = []

tvQ :: QType -> [Id]
tvQ (Forall x t) = filter (/= x) $ tvQ t
tvQ (Ty t) = tv t

tvGamma :: Gamma -> [Id]
tvGamma = nub . foldMap tvQ

infer :: Program -> Either TypeError Program
infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
                   return p'

unquantify :: QType -> TC Type
{-
Normally this implementation would be possible:

unquantify (Ty t) = return t
unquantify (Forall x t) = do x' <- fresh
                             unquantify (substQType (x =:x') t)

However as our "fresh" names are not checked for collisions with names bound in the type
we avoid capture entirely by first replacing each bound
variable with a guaranteed non-colliding variable with a numeric name,
and then substituting those numeric names for our normal fresh variables
-}

unquantify = unquantify' 0 emptySubst
unquantify' :: Int -> Subst -> QType -> TC Type
unquantify' i s (Ty t) = return $ substitute s t
unquantify' i s (Forall x t) = do x' <- fresh
                                  unquantify' (i + 1)
                                              ((show i =: x') <> s)
                                              (substQType (x =:TypeVar (show i)) t)

unify :: Type -> Type -> TC Subst

--1. both are type variables v1 and v2
unify (TypeVar v1) (TypeVar v2)
    | v1 == v2 = return emptySubst
    | otherwise = return (v1 =: (TypeVar v2))

--2. both are primitive types
unify (Base t1) (Base t2) 
    | t1 == t2 = return emptySubst
    | otherwise = typeError (TypeMismatch (Base t1) (Base t2))

--3. both are product types
unify (Prod t11 t12) (Prod t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify (substitute s t12) (substitute s t22)
    return (s <> s')

--4.function types and sum types
unify (Arrow t11 t12) (Arrow t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify (substitute s t12) (substitute s t22)
    return (s <> s')

unify (Sum t11 t12) (Sum t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify (substitute s t12) (substitute s t22)
    return (s <> s')

--5. only one is a type variable v, the other an arbitrary type term t
unify (TypeVar v) t
    | (elem v (tv t)) = typeError (OccursCheckFailed v t)
    | otherwise = return (v =: t)

unify t (TypeVar v)
    | (elem v (tv t)) = typeError (OccursCheckFailed v t)
    | otherwise = return (v =: t)

--6. otherwise, there is no unifier
unify x y = typeError (TypeMismatch x y)


generalise :: Gamma -> Type -> QType
generalise g t = foldr Forall (Ty t) ((tv t) \\ (tvGamma g))

inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
inferProgram env [Bind name _ [] exp] = do
    (exp', tau, tee)    <- inferExp env exp
    return ([Bind name (Just (generalise env tau)) [] (allTypes (substQType tee) exp')], tau, tee)
                                              
inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)

--Constants
inferExp g (Num n) = do
    return (Num n, Base Int, emptySubst)

--Variables
inferExp g (Var x) = do
    case E.lookup g x of
        Just tau    -> do
            tau'    <- unquantify tau
            return  (Var x, tau', emptySubst)
        Nothing     -> typeError (NoSuchVariable x)

--Constructors
inferExp g (Con c) = do
    case constType c of
        Just tau  -> do
            tau'  <- unquantify tau
            return  (Con c, tau', emptySubst)
        Nothing   -> typeError (NoSuchConstructor c)

--Primops
inferExp g (Prim op) = do
    case primOpType op of
      tau  -> do
        tau'    <- unquantify tau
        return  (Prim op, tau', emptySubst)

--Application (Structure from the lecture)
inferExp g (App e1 e2) = do
        (e1', tau1, tee)   <- inferExp g e1
        (e2', tau2, tee')  <- inferExp (substGamma tee g) e2
        alpha              <- fresh
        u                  <- unify (substitute tee' tau1) (Arrow tau2 alpha)
        return (App e1' e2', substitute u alpha , u <> tee' <> tee)

--If-Then-Else
inferExp g (If e e1 e2) = do
        (e', tau, tee)     <- inferExp g e
        u                  <- unify tau (Base Bool)
        (e1', tau1, tee1)  <- inferExp g e1
        (e2', tau2, tee2)  <- inferExp g e2
        u'                 <- unify (substitute tee2 tau1) (tau2)
        return (If e' e1' e2', substitute u' tau2, u' <> tee2 <> tee1 <> u <> tee)

--Case
inferExp g alts@(Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2]) = do
        (e', tau, tee)     <- inferExp g e
        alphaL             <- fresh
        (e1', tauL, tee1)  <- inferExp (E.add g (x, (Ty alphaL))) e1
        alphaR             <- fresh
        (e2', tauR, tee2)  <- inferExp (E.add g (y, (Ty alphaR))) e2 
        u                  <- unify (substitute (tee2 <> tee1 <> tee) (Sum alphaL alphaR)) (substitute (tee2 <> tee1) tau)
        u'                 <- unify (substitute (u <> tee2) tauL) (substitute u tauR)
        return (alts, substitute (u' <> u) tauR, u' <> u <> tee2 <> tee1 <> tee)

--Recursive Functions
inferExp g (Recfun (Bind f recType [x] e)) = do
        alpha1             <- fresh
        alpha2             <- fresh
        (e', tau, tee)     <- inferExp (E.addAll g [(x, Ty alpha1), (f, Ty alpha2)]) e 
        u                  <- unify (substitute tee alpha2) (Arrow (substitute tee alpha1) tau)
        let u'             = (substitute u (Arrow (substitute tee alpha1) tau))
        return (Recfun (Bind f (Just (Ty u')) [x] e'), u', u <> tee)

--Let Bindings
inferExp g (Let [(Bind n ty xs e1)] e2) = do
        (e1', tau, tee)    <- inferExp g e1
        let g'             = generalise (substGamma tee g) tau
        (e2', tau', tee')  <- inferExp (E.add (substGamma tee g) (n, g')) e2
        return ((Let [(Bind n (Just g') xs e1')] e2'), tau', (tee <> tee'))

