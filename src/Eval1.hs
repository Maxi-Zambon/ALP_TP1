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
update v x m = M.adjust (\i -> x) v m

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
stepComm c s = case of parseComm c
                  IfThenElse b c1 c2 ->let (b', s') =  (evalExp boolexp b) in case of 
                                                                              BTrue -> (c1, s')
                                                                              otherwise -> (c2, s')

                  IfThen b c          -> let (b', s') =  (evalExp boolexp b) in case of 
                                                                              BTrue -> (c1, s')
                                                                              otherwise -> (Skip, s')


                  RepeatUntil c b     -> (Seq c IfThenElse b Skip RepeatUntil b, s)

                  Skip                -> (Skip, s)

                  Seq Skip c1         -> (c1, s)

                  Seq c0 c1          -> let (c0', s') = stepComm c0 s in (Seq c0' c1, s')

                  Let v e             -> let (n, s') = evalExp (intexp e) in (Skip, update v n s')

                  otherwise           -> (c,s)
                                          

-- Evalúa una expresión
-- Completar la definición

evalExp :: Exp a -> State -> Pair a State
evalExp = undefined
