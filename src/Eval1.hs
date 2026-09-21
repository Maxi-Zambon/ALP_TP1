module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import Data.Bits (Bits(xor))

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor k m = m M.! k


-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v x m = M.insert v x m

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

--la llamada recursiva lo que hace basicamente es que stepComm me devuelve un estado con una commando y un estado
--resultante de evaluar un paso. Con uncurry permite que stepCommStar tome ese par como Comm -> State.

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm c s = case c of
                  IfThenElse b c1 c2 -> let (b' :!: s') =  (evalExp b s) in case b' of 
                                                                              True -> (c1 :!: s')
                                                                              False -> (c2 :!: s')



                  RepeatUntil c b     -> (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

                  Skip                -> (Skip :!: s)  

                  Seq Skip c1         -> (c1 :!: s)

                  Seq c0 c1          -> let (c0' :!: s') = stepComm c0 s in (Seq c0' c1 :!: s')

                  Let v e             -> let (n :!: s') = (evalExp e s) in (Skip :!: update v n s')

                                          

-- Evalúa una expresión
-- Completar la definición
--me llega una expresion booleana o entera parseada. Tengo que evaluarla.
evalExp :: Exp a -> State -> Pair a State
evalExp e s = case e of 
                Const nv -> (nv :!: s)

                Var v -> ((lookfor v s) :!: s) 

                UMinus f -> let (n :!: s') = (evalExp f s) in (-n :!: s')

                Plus e0 e1 -> let (n0 :!: s') = (evalExp e0 s)
                                  (n1 :!: s'') = (evalExp e1 s')
                                    in (n0+n1 :!: s'')

                Minus e0 e1 -> let (n0 :!: s') = (evalExp e0 s)
                                   (n1 :!: s'') = (evalExp e1 s')
                                     in (n0-n1 :!: s'')

                Times e0 e1 -> let (n0 :!: s') = (evalExp e0 s)
                                   (n1 :!: s'') = (evalExp e1 s')
                                     in (n0*n1 :!: s'')

                Div e0 e1 -> let (n0:!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in (n0 `div` n1 :!: s'')

                VarInc v -> let (n :!: s') = (evalExp (Var v) s) in (n+1 :!: (update v (n+1) s')) 

                VarDec v -> let (n :!: s') = (evalExp (Var v) s) in (n-1 :!: (update v (n-1) s')) 

                Eq e0 e1 -> let  (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                     in (n0 == n1 :!: s'')

                NEq e0 e1 -> let (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in (n0 /= n1 :!: s'')

                Lt e0 e1 -> let  (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in (n0 < n1 :!: s'')

                Gt e0 e1 -> let  (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in (n0 > n1 :!: s'')


                BTrue -> (True :!: s)

                BFalse -> (False :!: s)

                Not e -> let (b :!: s') = (evalExp e s) in (not b :!: s')

                And e0 e1 -> let (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in (n0 && n1 :!: s'')

                Or e0 e1 -> let  (n0 :!: s') = (evalExp e0 s)
                                 (n1 :!: s'') = (evalExp e1 s')
                                    in ((n0 || n1) :!: s'')                   