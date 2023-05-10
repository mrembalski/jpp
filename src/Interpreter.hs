{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State (StateT, execStateT, get, modify, put)
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

prettyBNFC :: BNFC'Position -> String
prettyBNFC p = case p of
  Just (l, c) -> "line " ++ show l ++ ", column " ++ show c
  Nothing -> "?"

interpreterError :: BNFC'Position -> String -> ReaderT Env Err Value
interpreterError p m = throwError ("error at " ++ prettyBNFC p ++ ":\n" ++ m)

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
eval (LitE _ n) = case n of
  IntL _ n -> return $ IntV n
  TL _ -> return $ BoolV True
  FL _ -> return $ BoolV False
eval (AddE p l o r) = do
  l' <- eval l
  r' <- eval r
  case (l', r') of
    (IntV l'', IntV r'') ->
      case o of
        AddO _ -> return $ IntV $ l'' + r''
        SubO _ -> return $ IntV $ l'' - r''
    _ -> interpreterError p "Invalid usage of '+' or '-' between different types."
eval (NegE p e) = do
  e' <- eval e
  case e' of
    IntV e'' -> return $ IntV $ -e''
    _ -> interpreterError p ("Invalid usage of '-' on " ++ show e' ++ ".")
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
    _ -> interpreterError p "Invalid usage of '*' or '/' between different types."
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
    _ -> interpreterError p "Invalid comparison between different types."
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
eval (IfE p cond t f) = do
  cond' <- eval cond
  case cond' of
    BoolV cond'' -> if cond'' then eval t else eval f
    _ -> interpreterError p ("Invalid condition: " ++ show cond')
eval (NotE p exp) = do
  exp' <- eval exp
  case exp' of
    BoolV exp'' -> return $ BoolV $ not exp''
    _ -> interpreterError p ("Invalid usage of 'not' on " ++ show exp')
eval (AppE _ f exp) = do
  val <- eval exp
  f' <- eval f
  case f' of
    FunV (LambdaF (Ident var) fun) env -> do
      case fun of
        ExpF exp' -> local (const $ insert var val env) (eval exp')
        LambdaF _ _ -> return $ FunV fun (insert var val env)
    _ -> interpreterError Nothing ("Cannot apply " ++ show f' ++ " to " ++ show val)
eval (VarE p (Ident var)) = do
  env <- ask
  case env !? var of
    Just val -> case val of
      FunV fun exp -> case fun of
        ExpF exp' -> local (const $ insert var val env) (eval exp')
        LambdaF _ _ -> return $ FunV fun (insert var val env)
      _ -> return val
    Nothing -> interpreterError p ("Variable " ++ var ++ " not found.")
eval (LmE p args exp) = do
  asks (FunV (foldr LambdaF (ExpF exp) args))
eval (LetE p ds exp) = do
  env <- ask
  let (Right env') = foldM (\e d -> execStateT (declare d) e) env ds
  local (const env') (eval exp)
eval (LstE p exps) = do
  exps' <- mapM eval exps
  case (null exps', allInts exps', allBools exps') of
    (True, _, _) -> return $ ListV EmptyList
    (_, True, _) -> return $ ListV $ IntListV $ map (\(IntV n) -> n) exps'
    (_, _, True) -> return $ ListV $ BoolListV $ map (\(BoolV b) -> b) exps'
    _ -> interpreterError p ("Invalid list: " ++ show exps')
eval (FunE p o e) = do
  e' <- eval e
  case e' of
    ListV (IntListV l) -> case o of
      Hd _ -> return $ IntV $ head l
      Tl _ -> return $ ListV $ IntListV $ tail l
      Em _ -> return $ BoolV $ null l
    ListV (BoolListV l) -> case o of
      Hd _ -> return $ BoolV $ head l
      Tl _ -> return $ ListV $ BoolListV $ tail l
      Em _ -> return $ BoolV $ null l
    ListV EmptyList -> case o of
      Em _ -> return $ BoolV True
      Tl _ -> return $ ListV EmptyList
      _ -> interpreterError p "Invalid operation on an empty list."
    _ -> interpreterError p "Invalid list operation on a non-list."
eval (ConE p e l) = do
  l' <- eval l
  e' <- eval e
  case (e', l') of
    (IntV e'', ListV (IntListV l'')) -> return $ ListV $ IntListV $ e'' : l''
    (IntV e'', ListV EmptyList) -> return $ ListV $ IntListV [e'']
    (BoolV e'', ListV (BoolListV l'')) -> return $ ListV $ BoolListV $ e'' : l''
    (BoolV e'', ListV EmptyList) -> return $ ListV $ BoolListV [e'']
    _ -> interpreterError p ("Cannot append " ++ show e' ++ " to " ++ show l' ++ ".")

declare :: Decl -> StateT Env Err ()
declare (FunD _ (Ident var) args exp) = do
  env <- get
  put $ insert var (FunV (foldr LambdaF (ExpF exp) args) env) env

interpret :: Program -> Err Env
interpret (P _ ds) = foldM (\e d -> execStateT (declare d) e) empty ds

evalMain :: Env -> Err Value
evalMain a = case a !? "main" of
  Just (FunV (ExpF exp) env) -> runReaderT (eval exp) env
  Just (FunV _ _) -> throwError "Main function should not be be parameterized."
  _ -> return $ IntV (-1)
