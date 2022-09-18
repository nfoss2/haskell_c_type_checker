module Main where

import ParCPP ( pProgram, myLexer )
import AbsCPP
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import System.Exit

main :: IO ()
main = do
  c <- getContents
  case pProgram (myLexer c) of
    Left err -> putStrLn "type error!" >> die err
    Right tree -> case typeCheck tree of
      Left s -> do
        putStrLn s
        exitFailure
      Right e -> putStrLn "Type Check Success"


data Env = Env (Map Id ((Type,Type),[Type])) [Map Id Type] deriving Show

emptyEnv ::Env
emptyEnv = Env Map.empty [Map.empty]

typeCheck :: Program -> Either String Env
typeCheck (PDefs defs) = do -- PDef
  env' <- declareFuns defs emptyEnv
  env'' <- typeCheckProgs defs env'
  return env''


typeCheckProgs :: [Def] -> Env -> Either String Env
typeCheckProgs [] env = return env -- PEDefs
typeCheckProgs (def:defs) env = do -- PDef
  env' <- typeCheckProg def env
  env'' <- typeCheckProgs defs env'
  return env''


typeCheckProg :: Def -> Env -> Either String Env
typeCheckProg (DFun tmain (Id ident) args []) (Env funcs ctx) = return (Env funcs ctx)
typeCheckProg (DFun tmain (Id ident) args stms) (Env funcs ctx) = do
  (Env funcs' ctx') <- typeCheckListStm stms (Env funcs (argMap args:ctx)) tmain (Id ident)
  case Map.lookup (Id ident) funcs' of
    Nothing -> Left "Something went very wrong"
    Just returnType ->
      if ident == "main" || snd (fst returnType) == tmain then
        -- Left $ "returnType: " ++ show returnType ++ ", tmain: " ++ show tmain
        Right (Env funcs' ctx')
      else
        Left "Function has incorrect return type and is not main"

argTypes :: [Arg] -> [Type]
argTypes = Prelude.map (\(ADecl t _) -> t)

argMap :: [Arg] -> Map Id Type
argMap [] = Map.empty
argMap ((ADecl t id):args) = Map.insert id t (argMap args)

declareFun :: Def -> Env -> Either String Env
declareFun (DFun t ident args stms) (Env funcs vars)
  | Map.member ident funcs = Left "function id already declared"
  | otherwise = Right $ Env (Map.insert ident ((t,Type_void), argTypes args) funcs) vars

declareFuns :: [Def] -> Env -> Either String Env
declareFuns [] env = return env
declareFuns (d:ds) env = do
  env' <- declareFun d env
  env'' <- declareFuns ds env'
  return env''


idsCheck :: Type -> [Id] -> Env -> Either String Env
idsCheck t [] env = Right env
idsCheck t (id:ids) (Env funcs vars)
  | Map.member id (head vars) = Left "id is already declared"
  | otherwise = do
    let
      vars' = Map.insert id t (head vars):tail vars in
        case idsCheck t ids (Env funcs vars') of
          Left s -> Left s
          Right e -> Right e


stmCheck :: Stm -> Env -> Type -> Id -> Either String Env
stmCheck stm (Env funcs vars) funT funcName = case stm of
  SExp exp -> do
    typeCheckExp exp (Env funcs vars)
    return (Env funcs vars)
  SDecls t ids -> do
    idsCheck t ids (Env funcs vars)
  SInit t id exp -> do
    if Map.member id (head vars) then Left "id already declared" else
      (do 
        let env' = Env funcs (Map.insert id t (head vars):tail vars)
        t' <- typeCheckExp exp env'
        if t == t' then Right env'
        else
          Left "types dont match"
      )
  SReturn exp -> do
    t <- typeCheckExp exp (Env funcs vars)
    if t == funT then do
      case Map.lookup funcName funcs of
        Nothing -> Left "something went very wrong"
        Just ((main,ret),types) -> do
          -- Left $ "func name: " ++ show funcName ++ ", exp: " ++ show exp ++ "func return type: " ++ show main ++ ", and types list: " ++ show types
          Right (Env (Map.insert funcName ((main,t),types) funcs) vars)
    else
      Left "return type does not match expected return type"
  SReturnVoid -> do
    if funT == Type_void then
      Right (Env funcs vars)
    else
      Left "return type id not void"
  SWhile exp stm' -> do
    t <- typeCheckExp exp (Env funcs vars)
    if t == Type_bool then do
      stmCheck stm' (Env funcs (Map.empty:vars)) funT funcName
      Right (Env funcs vars)
    else
      Left "exp was not of type bool"
  SBlock stms -> do
    (Env funcs' vars') <- typeCheckListStm stms (Env funcs (Map.empty:vars)) funT funcName
    return (Env funcs' (tail vars'))
  SIfElse exp stm1 stm2 -> do
    t <- typeCheckExp exp (Env funcs vars)
    if t == Type_bool then do
      stmCheck stm1 (Env funcs (Map.empty:vars)) funT funcName
      stmCheck stm2 (Env funcs (Map.empty:vars)) funT funcName
      Right (Env funcs vars)
    else
      Left "exp was not of type bool"

typeCheckListStm :: [Stm] -> Env -> Type -> Id -> Either String Env
typeCheckListStm [] env _ _ = return env -- SESeq
typeCheckListStm (s:ss) env t ident = do
  env' <- stmCheck s env t ident-- SSeq
  env'' <- typeCheckListStm ss env' t ident
  return env''

typeCheckExp :: Exp -> Env -> Either String Type
typeCheckExp exp env = case exp of
  ETrue -> Right Type_bool
  EFalse -> Right Type_bool
  EInt x -> Right Type_int
  EDouble x -> Right Type_double
  EString x -> Right Type_string
  EId id -> lookupVar id env
  EApp id exps -> do
    t <- lookupFunc id env
    argTypes <- getFuncArgTypes id env
    paramTypes <- typeCheckExpList exps env
    if compLists argTypes paramTypes then
      Right t
    else
      Left $ "application failed, " ++ show id ++ " type: " ++ show t ++ ", argtypes: " ++ show argTypes ++ ", paramTypes: " ++ show paramTypes ++ ", env: " ++ show env
  EPIncr e -> do
    t <- typeCheckExp e env
    if t == Type_int || t == Type_double then
      Right t
    else
      Left "increment failed"
  EPDecr e -> do
    t <- typeCheckExp e env
    if t == Type_int || t == Type_double then
      Right t
    else
      Left "decrement failed"
  EIncr e -> do
    t <- typeCheckExp e env
    if t == Type_int || t == Type_double then
      Right t
    else
      Left "increment failed"
  EDecr e -> do
    t <- typeCheckExp e env
    if t == Type_int || t == Type_double then
      Right t
    else
      Left "decrement failed"
  ETimes e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 || checkTypesEqualTo Type_double t1 t2 then
      Right t1
    else
      Left "multiplication failed"
  EDiv e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 || checkTypesEqualTo Type_double t1 t2 then
      Right t1
    else
      Left "division failed"
  EPlus e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 || checkTypesEqualTo Type_double t1 t2 || checkTypesEqualTo Type_string t1 t2 then
      Right t1
    else
      Left "addition failed"
  EMinus e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 || checkTypesEqualTo Type_double t1 t2 then
      Right t1
    else
      Left "subtraction failed"
  ELt e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "less than failed"
  EGt e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "greater than failed"
  ELtEq e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "less than or equal to failed"
  EGtEq e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "greater than or equal failed"
  EEq e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "equal failed"
  ENEq e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_int t1 t2 ||
      checkTypesEqualTo Type_double t1 t2 ||
      checkTypesEqualTo Type_bool t1 t2 ||
      checkTypesEqualTo Type_string t1 t2 then
      Right Type_bool
    else
      Left "not equal failed"
  EAnd e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_bool t1 t2 then
      Right Type_bool
    else
      Left "and failed"
  EOr e1 e2 -> do
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if checkTypesEqualTo Type_bool t1 t2 then
      Right Type_bool
    else
      Left "or failed"
  EAss e1 e2 -> do
    -- t1 <- lookupVar id env
    t1 <- typeCheckExp e1 env
    t2 <- typeCheckExp e2 env
    if t1 == t2 then
      Right t1
    else
      Left $ "assignment failed, exp1: " ++ show t1 ++ ", exp2: " ++ show t2 ++ ", whole thing: " ++ show e1 ++ " " ++ show e2

-- source: https://over.wiki/ask/the-function-of-comparing-2-lists-in-haskell/
compLists :: [Type] -> [Type] -> Bool
compLists (arg:args) (param:params) = (arg == param) && compLists args params
compLists [] [] = True 
compLists _ _ = False

getFuncArgTypes :: Id -> Env -> Either String [Type]
getFuncArgTypes (Id ident) (Env funcs vars)
  | Map.member (Id ident) funcs = do
      case Map.lookup (Id ident) funcs of
        Just a -> Right (snd a)
        Nothing -> Left "function not found"
  | otherwise = Left "error"

typeCheckExpList :: [Exp] -> Env -> Either String [Type]
typeCheckExpList [] env = Right []
typeCheckExpList (e:es) env = do
  t <- typeCheckExp e env
  ts <- typeCheckExpList es env
  return (t:ts)

lookupVar :: Id -> Env -> Either String Type
lookupVar _ (Env funcs []) = Left "variable not found"
lookupVar ident (Env funcs (ctx:vars))
  | Map.member ident ctx = do
      case Map.lookup ident ctx of
        Just a -> return a
        Nothing -> Left "variable not found"
  | otherwise = lookupVar ident (Env funcs vars)

lookupFunc :: Id -> Env -> Either String Type
lookupFunc (Id ident) (Env funcs vars)
  | Map.member (Id ident) funcs = do
      case Map.lookup (Id ident) funcs of
        Just a -> Right (fst (fst a))
        Nothing -> Left "function not found"
  | otherwise = Left "error"

-- varExistsCheck :: Id -> Env -> Bool
-- varExistsCheck id (Env _ (ctx:vars))
--   | Map.member id ctx = True
--   | otherwise = False

checkTypesEqualTo :: Type -> Type -> Type -> Bool
checkTypesEqualTo t t1 t2 =
  (t == t1) && (t == t2)