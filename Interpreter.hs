{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Lazy (Map, empty, insert, (!?))
import Ruskell.Abs
import Ruskell.ErrM

data List = IntListV [Integer] | BoolListV [Bool] | EmptyList
  deriving (Eq)

instance Show List where
  show (IntListV il) = show il
  show (BoolListV bl) = show bl
  show EmptyList = "[]"

data Lambda = ExpF Exp | LambdaF Ident Lambda
  deriving (Eq)

instance Show Lambda where
  show (ExpF e) = show e
  show (LambdaF (Ident x) e) = "<function>"

data Value = BoolV Bool | IntV Integer | ListV List | FunV Lambda Env
  deriving (Eq)

instance Show Value where
  show (IntV n) = show n
  show (ListV il) = show il
  show (BoolV b) = show b
  show (FunV f e) = show f

type Env = Map String Value

-- nicer-looking error messages
prettyBNFC :: BNFC'Position -> String
prettyBNFC p = case p of
  Just (l, c) -> "line " ++ show l ++ ", column " ++ show c
  Nothing -> "?"

interpreterError :: BNFC'Position -> String -> ReaderT Env Err Value
interpreterError p m = throwError ("error at " ++ prettyBNFC p ++ ":\n" ++ m)

-- helper functions
allInts :: [Value] -> Bool
allInts =
  all
    ( \case
        IntV _ -> True
        _ -> False
    )

allBools :: [Value] -> Bool
allBools =
  all
    ( \case
        BoolV _ -> True
        _ -> False
    )

eval :: Exp -> ReaderT Env Err Value
-- literals and simple expressions
eval (LitE _ n) = case n of
  IntL _ n -> return $ IntV n
  TL _ -> return $ BoolV True
  FL _ -> return $ BoolV False
eval (NotE p e) = do
  e' <- eval e
  case e' of
    BoolV e'' -> return $ BoolV $ not e''
    _ -> interpreterError p ("Invalid usage of 'not' on " ++ show e')
eval (NegE p e) = do
  e' <- eval e
  case e' of
    IntV e'' -> return $ IntV $ -e''
    _ -> interpreterError p ("Invalid usage of '-' on " ++ show e' ++ ".")
eval (OrE p l r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (BoolV l'', BoolV r'') -> return $ BoolV $ l'' || r''
    _ -> interpreterError p ("Cannot use '||' between " ++ show l' ++ " and " ++ show r' ++ ".")
eval (AndE p l r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (BoolV l'', BoolV r'') -> return $ BoolV $ l'' && r''
    _ -> interpreterError p ("Cannot use '&&' between " ++ show l' ++ " and " ++ show r' ++ ".")
eval (AddE p l o r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (IntV l'', IntV r'') ->
      case o of
        AddO _ -> return $ IntV $ l'' + r''
        SubO _ -> return $ IntV $ l'' - r''
    _ -> interpreterError p "Invalid usage of '+' or '-' between not-integers."
eval (MulE p l o r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (IntV l'', IntV r'') ->
      case o of
        MulO _ -> return $ IntV $ l'' * r''
        DivO p' -> case r'' of
          0 -> interpreterError p' "Division by zero."
          _ -> return $ IntV $ l'' `div` r''
    _ -> interpreterError p "Invalid usage of '*' or '/' between not-integers."
eval (RelE p l o r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (IntV l'', IntV r'') ->
      case o of
        LtO _ -> return $ BoolV $ l'' < r''
        LeO _ -> return $ BoolV $ l'' <= r''
        GtO _ -> return $ BoolV $ l'' > r''
        GeO _ -> return $ BoolV $ l'' >= r''
        EqO _ -> return $ BoolV $ l'' == r''
        NeO _ -> return $ BoolV $ l'' /= r''
    (BoolV l'', BoolV r'') ->
      case o of
        EqO _ -> return $ BoolV $ l'' == r''
        NeO _ -> return $ BoolV $ l'' /= r''
        _ -> interpreterError p "Invalid comparison between bools."
    (ListV l'', ListV r'') ->
      case (l'', r'') of
        (IntListV l''', IntListV r''') ->
          case o of
            EqO _ -> return $ BoolV $ l''' == r'''
            NeO _ -> return $ BoolV $ l''' /= r'''
            _ -> interpreterError p "Invalid comparison between lists."
        (BoolListV l''', BoolListV r''') ->
          case o of
            EqO _ -> return $ BoolV $ l''' == r'''
            NeO _ -> return $ BoolV $ l''' /= r'''
            _ -> interpreterError p "Invalid comparison between lists."
        (EmptyList, EmptyList) ->
          case o of
            EqO _ -> return $ BoolV True
            NeO _ -> return $ BoolV False
            _ -> interpreterError p "Invalid comparison between lists."
        _ -> interpreterError p "Invalid comparison between lists of different types."
    (FunV l'' _, FunV r'' _) ->
      interpreterError p "Cannot compare functions."
    _ -> interpreterError p "Invalid comparison between different types."

-- variables
eval (VarE p (Ident var)) = do
  env <- ask
  case env !? var of
    Just val -> case val of
      FunV fun env' -> case fun of
        ExpF exp -> local (const $ insert var val env') (eval exp)
        LambdaF _ _ -> return $ FunV fun (insert var val env')
      _ -> return val
    Nothing -> interpreterError p ("Variable " ++ var ++ " not found.")

-- if
eval (IfE p c t f) = do
  c' <- eval c
  case c' of
    BoolV c'' -> if c'' then eval t else eval f
    _ -> interpreterError p ("Invalid condition: " ++ show c')

-- lambda & application
eval (LmE p args e) = do
  asks (FunV (foldr LambdaF (ExpF e) args))
eval (AppE p l r) = do
  r' <- eval r
  l' <- eval l
  case l' of
    -- left side must be a function
    -- application "takes off" one variable from the lambda
    FunV (LambdaF (Ident var) (ExpF exp)) env -> local (const $ insert var r' env) (eval exp)
    FunV (LambdaF (Ident var) lm) env -> return $ FunV lm (insert var r' env)
    _ -> interpreterError p ("Cannot apply (left: " ++ show l' ++ ") to (right: " ++ show r' ++ ").")

-- let
eval (LetE p ds e) = do
  env <- ask
  let (Right env') = execStateT (mapM_ declare ds) env
  local (const env') (eval e)

-- lists
eval (LstE p exps) = do
  exps' <- mapM eval exps
  case (null exps', allInts exps', allBools exps') of
    (True, _, _) -> return $ ListV EmptyList
    (_, True, _) -> return $ ListV $ IntListV $ map (\(IntV n) -> n) exps'
    (_, _, True) -> return $ ListV $ BoolListV $ map (\(BoolV b) -> b) exps'
    _ -> interpreterError p ("Invalid list: " ++ show exps')
eval (ConE p e l) = do
  l' <- eval l
  e' <- eval e
  case (e', l') of
    (IntV e'', ListV (IntListV l'')) -> return $ ListV $ IntListV $ e'' : l''
    (IntV e'', ListV EmptyList) -> return $ ListV $ IntListV [e'']
    (BoolV e'', ListV (BoolListV l'')) -> return $ ListV $ BoolListV $ e'' : l''
    (BoolV e'', ListV EmptyList) -> return $ ListV $ BoolListV [e'']
    _ -> interpreterError p ("Cannot append " ++ show e' ++ " to " ++ show l' ++ ".")

-- builtin functions
eval (FunE p o e) = do
  e' <- eval e
  case e' of
    ListV (IntListV l) -> case o of
      Hd _ -> case l of
        [] -> interpreterError p "Cannot get head of an empty list."
        (x : _) -> return $ IntV x
      Tl _ -> case l of
        [] -> interpreterError p "Cannot get tail of an empty list."
        (_ : xs) -> return $ ListV $ IntListV xs
      Em _ -> return $ BoolV $ null l
    ListV (BoolListV l) -> case o of
      Hd _ -> case l of
        [] -> interpreterError p "Cannot get head of an empty list."
        (x : _) -> return $ BoolV x
      Tl _ -> case l of
        [] -> interpreterError p "Cannot get tail of an empty list."
        (_ : xs) -> return $ ListV $ BoolListV xs
      Em _ -> return $ BoolV $ null l
    ListV EmptyList -> case o of
      Hd _ -> interpreterError p "Cannot get head of an empty list."
      Tl _ -> interpreterError p "Cannot get tail of an empty list."
      Em _ -> return $ BoolV True
    _ -> interpreterError p "Invalid list operation on a non-list."

declare :: Decl -> StateT Env Err ()
declare (FunD _ (Ident var) args exp) = do
  env <- get
  put $ insert var (FunV (foldr LambdaF (ExpF exp) args) env) env

initialEnv :: Env
initialEnv = empty

interpret :: Program -> Err Env
interpret (P _ ds) = execStateT (mapM_ declare ds) initialEnv

evalMain :: Env -> Err Value
evalMain env = case env !? "main" of
  Just main -> case main of
    FunV (ExpF exp) env' -> runReaderT (eval exp) env'
    _ -> throwError "Main function incorrect - invalid declaration or parameterized."
  Nothing -> throwError "Main function not found."
