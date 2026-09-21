module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

type State = M.Map Variable Int

initState :: State
initState = M.empty

lookfor :: Variable -> State -> Either Error Int
lookfor k m = case M.lookup k m of
  Just v  -> Right v
  Nothing -> Left UndefVar

update :: Variable -> Int -> State -> State
update v x m = M.insert v x m

eval :: Comm -> Either Error State
eval p = stepCommStar p initState

stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = Right s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm c s = case c of
  IfThenElse b c1 c2 -> do
    (b' :!: s') <- evalExp b s
    case b' of
      True  -> Right (c1 :!: s')
      False -> Right (c2 :!: s')

  RepeatUntil c b -> Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

  Skip -> Right (Skip :!: s)

  Seq Skip c1 -> Right (c1 :!: s)

  Seq c0 c1 -> do
    (c0' :!: s') <- stepComm c0 s
    Right (Seq c0' c1 :!: s')

  Let v e -> do
    (n :!: s') <- evalExp e s
    Right (Skip :!: update v n s')

evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp e s = case e of
  Const nv -> Right (nv :!: s)

  Var v -> do
    n <- lookfor v s
    Right (n :!: s)

  UMinus f -> do
    (n :!: s') <- evalExp f s
    Right (-n :!: s')

  Plus e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 + n1 :!: s'')

  Minus e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 - n1 :!: s'')

  Times e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 * n1 :!: s'')

  Div e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    if n1 == 0
      then Left DivByZero
      else Right (n0 `div` n1 :!: s'')

  VarInc v -> do
    (n :!: s') <- evalExp (Var v) s
    Right (n + 1 :!: update v (n + 1) s')

  VarDec v -> do
    (n :!: s') <- evalExp (Var v) s
    Right (n - 1 :!: update v (n - 1) s')

  Eq e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 == n1 :!: s'')

  NEq e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 /= n1 :!: s'')

  Lt e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 < n1 :!: s'')

  Gt e0 e1 -> do
    (n0 :!: s')  <- evalExp e0 s
    (n1 :!: s'') <- evalExp e1 s'
    Right (n0 > n1 :!: s'')

  BTrue -> Right (True :!: s)

  BFalse -> Right (False :!: s)

  Not e -> do
    (b :!: s') <- evalExp e s
    Right (not b :!: s')

  And e0 e1 -> do
    (b0 :!: s')  <- evalExp e0 s
    (b1 :!: s'') <- evalExp e1 s'
    Right (b0 && b1 :!: s'')

  Or e0 e1 -> do
    (b0 :!: s')  <- evalExp e0 s
    (b1 :!: s'') <- evalExp e1 s'
    Right ((b0 || b1) :!: s'')