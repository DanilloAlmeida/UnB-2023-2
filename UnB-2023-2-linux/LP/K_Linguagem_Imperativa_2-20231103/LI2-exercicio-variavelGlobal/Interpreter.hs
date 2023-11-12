module Interpreter where

import AbsLI
import Prelude hiding (lookup)

executeP :: Program -> Environment

executeP (Prog declarations) =  execute (updatecF initialEnvironmentMain fs) (SBlock initializationCommand)
    where stmMain ((Fun (Ident "main") decls stms):xs) = stms
          stmMain ( _ :xs) = stmMain xs
          (gvs,fs) = foldl (\(gvs,fs) decl  -> case decl of 
                                                 DecF  f   -> (gvs,f:fs)
                                                 DecGV gv  -> (gv:gvs,fs)
                           ) ([],[]) declarations
          initializationCommand = (map (\e -> SAss e) gvs) ++ (stmMain fs)
          initialEnvironmentMain = push initialEnvironment
          
   
execute :: Environment -> Stm -> Environment
execute env x = case x of
   SAss (SAsss  id exp) -> let (valor,nenv) = eval env exp  in  
                              updateDeepValue nenv id valor
   SBlock [] -> env 
   SBlock (s:stms) -> execute (execute env s) (SBlock stms) 
   SWhile exp stm -> let (valor,nenv) = eval env exp in 
                        if ( i (valor) /= 0) 
                         then execute (execute nenv stm) (SWhile exp stm)
                         else nenv
   SReturn exp ->  let (valor,nenv) = eval env exp in 
                      updateShallowValue nenv  (Ident "return")  valor
   SIf exp stmT stmE -> let (valor,nenv) = eval env exp in 
                         if ( i (valor) /= 0) 
                           then execute nenv stmT
                           else execute nenv stmE
 


eval :: Environment -> Exp -> (Valor,Environment)
eval env x = case x of
    ECon exp0 exp  -> (ValorStr ( (s valor1) ++  (s valor2)), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                           (valor2,nenv2) = (eval nenv1 exp)
    EAdd exp0 exp  -> (ValorInt ( (i valor1) + (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    ESub exp0 exp  -> (ValorInt ( (i valor1) - (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    EMul exp0 exp  -> (ValorInt ( (i valor1) * (i valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                          (valor2,nenv2) = (eval nenv1 exp)
    EDiv exp0 exp  -> (ValorInt ( (i valor1) `div` (i valor2) ), nenv2)  where (valor1,nenv1) = (eval env exp0)
                                                                               (valor2,nenv2) = (eval nenv1 exp)
    EOr  exp0 exp  -> (ValorBool ( (b valor1) || (b valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                            (valor2,nenv2) = (eval nenv1 exp)
    EAnd exp0 exp  -> (ValorBool ( (b valor1) && (b valor2) ), nenv2) where (valor1,nenv1) = (eval env exp0)
                                                                            (valor2,nenv2) = (eval nenv1 exp)
    ENot exp0       -> (ValorBool (not (b valor1)), nenv1) where (valor1,nenv1) = (eval env exp0)
                                                                           
    EStr str       -> (ValorStr str, env)
    ETrue          -> (ValorBool True, env)
    EFalse         -> (ValorBool False, env)
    EInt n         -> (ValorInt n, env )
    EVar id        -> (lookupDeepValue env  id, env)
    Call id lexp   -> (lookupShallowValue nenv (Ident "return"),pushB localContext (pop nenv)) 
                       where ValorFun (Fun _ decls stms) = lookupShallowFunction env id 
                             paramBindings = zip decls (map (\arg -> fst (eval env arg)) lexp)
                             localContext = top env
                             executionEnv = pushB paramBindings (pop env)
                             nenv = execute executionEnv(SBlock stms)
                             
                           
    

data Valor = ValorInt {
               i :: Integer         
             }
            | 
             ValorFun {
               f :: Function
             }   
            | 
             ValorStr {
               s :: String
             } 
            | ValorBool {
               b :: Bool
             }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = show s
  show (ValorFun (Fun nf decls _)) = show (nf) ++ "[" ++ show (decls) ++ "]" 
  
--(\(Ident x) -> x) nf

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)



type Environment = ([RContext],RContext)
type RContext = [(Ident,Valor)]

initialEnvironment = ([],[])

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx) = (typeBindings:sc,fnCtx) 

push :: Environment -> Environment
push (sc,fnCtx) = ([]:sc,fnCtx)
 
pop :: Environment -> Environment
pop ((s:scs),fnCtx) = (scs,fnCtx)

top :: Environment -> RContext
top ((s:scs),fnCtx) = s


lookupDeepValueA :: Environment -> Ident -> R Valor
lookupDeepValueA ([],fnCtx) id = Erro (show id ++ " nao esta no contexto. ")
lookupDeepValueA ((s:scs),fnCtx) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> OK val
                                            Erro _ -> lookupDeepValueA (scs,fnCtx) id


lookupDeepValue :: Environment -> Ident -> Valor
lookupDeepValue ((s:scs),fnCtx) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> val
                                            Erro _ -> lookupDeepValue (scs,fnCtx) id

lookupShallowValue :: Environment -> Ident -> Valor   
lookupShallowValue  ((s:sc),_) id =  (\(OK val) -> val) (lookupShallow s id)
                                      
lookupShallowFunction :: Environment -> Ident -> Valor
lookupShallowFunction (_,fnCtx) id = (\(OK val) -> val) (lookupShallow fnCtx id)

lookupShallow :: RContext -> Ident -> R Valor
lookupShallow [] s = Erro (show s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s =  OK v
   | otherwise = lookupShallow cs s

updateShallowValue :: Environment -> Ident -> Valor -> Environment
updateShallowValue ([],fnCtx) id tp = ([[(id,tp)]],fnCtx)
updateShallowValue ((s:sc),fnCtx) id tp = ( (updateShallow s id tp):sc , fnCtx)   

updateDeepValue :: Environment -> Ident -> Valor -> Environment
updateDeepValue ([],fnCtx) id tp = ([[(id,tp)]],fnCtx)
updateDeepValue ((s:sc),fnCtx) id tp = let r = lookupShallow s id in 
                                           case r of
                                               OK _ -> ( (updateShallow s id tp):sc , fnCtx)
                                               Erro _ -> pushB s (updateDeepValue (sc,fnCtx) id tp)    
                                             
updateShallow :: RContext -> Ident -> Valor -> RContext
updateShallow [] s nv = [(s,nv)]
updateShallow ((i,v):cs) s nv
        | i == s = (i,nv):cs
        | otherwise = (i,v) : updateShallow cs s nv
 
updatecF :: Environment -> [Function] -> Environment
updatecF e [] = e
updatecF (sc,c) (f@(Fun id params stms):fs) = updatecF (sc, updateShallow c id (ValorFun f)) fs
                                                     



