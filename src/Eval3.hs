module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (initState = M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor k (m,_) = case M.lookup k m of
  Just v  -> Right v
  Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v x (m,s) = (M.insert v x m,s)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace traza (m,s) = (m, s ++ " " ++ traza)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm c s = case c of
  IfThenElse b c1 c2 -> case evalExp b s of
    Left err         -> Left err
    Right (b' :!: s') -> case b' of
      True  -> Right (c1 :!: s')
      False -> Right (c2 :!: s')

  RepeatUntil c b -> Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

  Skip -> Right (Skip :!: s)

  Seq Skip c1 -> Right (c1 :!: s)

  Seq c0 c1 -> case stepComm c0 s of
    Left err             -> Left err
    Right (c0' :!: s')   -> Right (Seq c0' c1 :!: s')

  Let v e -> case evalExp e s of
    Left err        -> Left err
    Right (n :!: s') -> Right (Skip :!: update v n s')

evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp e s = case e of
  Const nv -> Right (nv :!: s)

  Var v -> case lookfor v s of
    Left err -> Left err
    Right n  -> Right (n :!: s)

  UMinus f -> case evalExp f s of
    Left err       -> Left err
    Right (n :!: s') -> Right (-n :!: s')

  Plus e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 + n1 :!: s'')

  Minus e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 - n1 :!: s'')

  Times e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 * n1 :!: s'')

  Div e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> if n1 == 0
        then Left DivByZero
        else Right (n0 `div` n1 :!: s'')

  VarInc v -> case evalExp (Var v) s of
    Left err         -> Left err
    Right (n :!: s') -> Right (n + 1 :!: update v (n + 1) s')

  VarDec v -> case evalExp (Var v) s of
    Left err         -> Left err
    Right (n :!: s') -> Right (n - 1 :!: update v (n - 1) s')

  Eq e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 == n1 :!: s'')

  NEq e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 /= n1 :!: s'')

  Lt e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 < n1 :!: s'')

  Gt e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (n0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (n1 :!: s'') -> Right (n0 > n1 :!: s'')

  BTrue -> Right (True :!: s)

  BFalse -> Right (False :!: s)

  Not e -> case evalExp e s of
    Left err       -> Left err
    Right (b :!: s') -> Right (not b :!: s')

  And e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (b0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (b1 :!: s'') -> Right (b0 && b1 :!: s'')

  Or e0 e1 -> case evalExp e0 s of
    Left err           -> Left err
    Right (b0 :!: s')  -> case evalExp e1 s' of
      Left err           -> Left err
      Right (b1 :!: s'') -> Right ((b0 || b1) :!: s'')
