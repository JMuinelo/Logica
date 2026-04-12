module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Joaquín Moreira y Juan Martin Muinelo
-- Números: 357603 y 350499
----------------------------------------------------
import Data.List
import Prelude


-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
x = V "x"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg f) = not (eval i f)
eval i (Bin f And g) = eval i f && eval i g
eval i (Bin f Or g) = eval i f || eval i g
eval i (Bin f Imp g) = not (eval i f) || eval i g
eval i (Bin f Iff g) = eval i f == eval i g

--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas = \x -> True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas = \y -> False

--1.4)
irfalsa :: Var -> Bool
irfalsa "r" = False
irfalsa _ = True

--1.5)
-- Completar con verdadera/falsa:
-- fa es False bajo itodasfalsas
-- fb es False bajo itodasfalsas
-- fc es True  bajo itodasfalsas
-- fd es False bajo itodasfalsas
-- 
-- fa es True bajo itodasverdaderas
-- fb es False bajo itodasverdaderas
-- fc es True bajo itodasverdaderas
-- fd es False bajo itodasverdaderas
--
-- fa es True bajo irfalsa
-- fb es False bajo irfalsa
-- fc es True  bajo irfalsa
-- fd es True bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari [] x = False
creari ((v,b):xs) x 
    | x == v    = b
    | otherwise = creari xs x

--1.7)
-- Si, es igual si trabajamos en el contexto de p q r porque se asigna false a r y true a p y q.


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas [] = [[]]
filas (x:xs) = agregar x (filas xs)

agregar :: Var -> [Fila] -> [Fila]
agregar x [] = []
agregar x (f:fs) = ((x,False):f) : ((x,True):f) : agregar x fs


--2.2)
tv :: L -> TV
tv f = armarTabla f (filas (variables f))

armarTabla :: L -> [Fila] -> TV
armarTabla f [] = []
armarTabla f (y:ys) = (y, eval (creari y) f) : armarTabla f ys

variables :: L -> [Var]
variables (V x) = [x]
variables (Neg f) = variables f
variables (Bin f _ g) = nub (variables f ++ variables g)

--2.3)
es :: L -> Clase -> Bool
es f c = pertenece c (tv f)

pertenece :: Clase -> TV -> Bool
pertenece Tau [] = True
pertenece Tau ((fila,b):xs) = b && pertenece Tau xs

pertenece Contra [] = True
pertenece Contra ((fila,b):xs) = not b && pertenece Contra xs

pertenece Sat [] = False
pertenece Sat ((fila,b):xs) = b || pertenece Sat xs

pertenece Fal [] = False
pertenece Fal ((fila,b):xs) = not b || pertenece Fal xs

pertenece Cont tv = pertenece Sat tv && pertenece Fal tv

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es contingencia
-- fb es contingencia
-- fc es una Teutología
-- fd es una Contradicción

--2.5) 
fnc :: L -> L
fnc f = conjuncion (map filaAdisyuncion (filasFalsas (tv f)) )

filasFalsas :: TV -> [Fila]
filasFalsas [] = []
filasFalsas ((f,b):xs)
  | b == False = f : filasFalsas xs
  | otherwise      = filasFalsas xs

filaAdisyuncion :: Fila -> L
filaAdisyuncion [(x,True)]  = Neg (V x)
filaAdisyuncion [(x,False)] = V x
filaAdisyuncion ((x,True):xs)  = Bin (Neg (V x)) Or (filaAdisyuncion xs)
filaAdisyuncion ((x,False):xs) = Bin (V x) Or (filaAdisyuncion xs)

conjuncion :: [L] -> L
conjuncion [] = Bin (V "p") Or (Neg (V "p"))
conjuncion [f] = f
conjuncion (f:fs) = Bin f And (conjuncion fs)



----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"
