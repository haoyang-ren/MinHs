  module MinHS.Evaluator where
  import qualified MinHS.Env as E
  import MinHS.Syntax
  import MinHS.Pretty
  import qualified Text.PrettyPrint.ANSI.Leijen as PP
  import Debug.Trace

  type VEnv = E.Env Value

  data FunV = FunV (Value -> Value) 
  instance Show FunV where
      show _ = "<function>"

  data Value = I Integer
              | B Bool
              | Nil
              | Cons Integer Value
              | Closure VEnv Bind
              | Function FunV
              -- Others as needed
              deriving (Show)
  
  instance PP.Pretty Value where
    pretty (I i) = numeric $ i
    pretty (B b) = datacon $ show b
    pretty (Nil) = datacon "Nil"
    pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
    pretty _ = undefined -- should not ever be used
  
  evaluate :: Program -> Value
  evaluate [Bind _ _ _ e] = evalE E.empty e
  evaluate bs = evalE E.empty (Let bs (Var "main"))

  evalE :: VEnv -> Exp -> Value
  -- Basic Case
  evalE env (Num n) = I n
  evalE env (Con "True") = B True
  evalE env (Con "False") = B False

  -- Listops Case
  evalE env (Con "Nil") = Nil
  
  evalE env (App (App (Con "Cons") v1) v2) =
    let I n = evalE env v1
    in Cons n (evalE env v2)
  
  evalE env (App (Prim Head) v) =
    let Cons head x = evalE env v
    in I head
  
  evalE env (App (Prim Tail) v) =
    let Cons x tail = evalE env v
    in tail
    
  evalE env (App (Prim Neg) v) = 
    let I e = evalE env v
    in I (negate e) 
  
  evalE env (App (Prim Null) v) = case (evalE env v) of
                                       (Nil)     -> B True
                                       otherwise -> B False

  -- Ifthenelse case                                    
  evalE env (If e1 e2 e3) = case evalE env e1 of
                                 B True    -> evalE env e2
                                 I _       -> error ("Type requirement error")
                                 otherwise -> evalE env e3


  -- Lookup Variables
  evalE env (Var vname) = case E.lookup env vname of
                               Just v  -> v
                               Nothing -> error ("Error, not in scope variable: " ++ vname)
 
  -- Integer PrimOp
  evalE env (Prim Add) = intOperator (+)
  evalE env (Prim Sub) = intOperator (-)
  evalE env (Prim Mul) = intOperator (*)
  evalE env (Prim Quot) = intOperator div
  evalE env (Prim Rem) = intOperator rem
  
  -- Boolean PrimOp
  evalE env (Prim Gt) = boolOperator (>)
  evalE env (Prim Lt) = boolOperator (<)
  evalE env (Prim Ge) = boolOperator (>=)
  evalE env (Prim Le) = boolOperator (<=)
  evalE env (Prim Eq) = boolOperator (==)
  evalE env (Prim Ne) = boolOperator (/=)

  -- Application function
  evalE env (App v1 v2) = case evalE env v1 of
                               Function (FunV f) -> f (evalE env v2)

  -- Recursive function binding closures
  evalE env (Recfun (Bind vname t args exp)) = 
    let
      env' = E.add env (vname, (evalE env (Recfun (Bind vname t args exp))))
    in 
      closureFunc env' args exp

  -- Multiple variables binding in let
  evalE env (Let binds exp) =
    let
      binding [] env' = env'
      binding ((Bind vname _ args exp):bs) env' = binding bs (E.add env' (vname, (closureFunc env' args exp)))
    in
      evalE (binding binds env) exp      

  -- Mutually recursive bindings
  evalE env (Letrec binds exp) = 
    let
      env' = E.addAll env (map f binds)
      f (Bind vname _ args body) = (vname, closureFunc env' args body)
    in
      evalE env' exp
  

  boolOperator :: (Integer -> Integer -> Bool) -> Value
  boolOperator sign = Function (FunV (\(I e1) -> Function (FunV (\(I e2) -> (B (e1 `sign` e2))))))
  
  intOperator :: (Integer -> Integer -> Integer) -> Value
  intOperator sign = Function (FunV (\(I e1) -> Function (FunV (\(I e2) -> (I (e1 `sign` e2))))))
  
  closureFunc :: VEnv -> [Id] -> Exp -> Value
  closureFunc env [] exp = evalE env exp
  closureFunc env (x:xs) exp = Function (FunV (\e -> closureFunc (E.add env (x, e)) (xs) exp))
  
