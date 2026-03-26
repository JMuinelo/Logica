-- docker run --rm --volume "$(pwd)/:/myFiles" -it haskell
-- :l myFiles/bool.hs

module Practico1 where

type Prop = String

-- data L = P Prop
--          | Neg L
--          | And L L
--          | Or L L
--          | Imp L L
--          | Iff L L

-- f2b :: Form
-- f2b = ((P "p") `Or` (P "p")) `Imp` (((P "q") `Imp` (P "r")) `And` (P "r"))


data Form = P Prop
            | Neg Form
            | Bin BinCon Form Form
            deriving Show

data BinCon = Or | And | Imp | Iff
            deriving Show

-- ============================================
-- Formulas de prueba (ejercicio 4.2)
-- ============================================

-- a. neg p ⊃ q /\ r
f2a :: Form
f2a = Bin Imp (Neg (P "p")) (Bin And (P "q") (P "r"))

-- b. p \/ p ⊃ (q ⊃ r) /\ r
f2b :: Form
f2b = Bin Imp (Bin Or (P "p") (P "p")) (Bin And (Bin Imp (P "q") (P "r")) (P "r"))

-- c. p ⊃ q /\ p ⊃ q
f2c :: Form
f2c = Bin Imp (P "p") (Bin Imp (Bin And (P "q") (P "p")) (P "q"))

-- d. p /\ (p ⊃ q) ⊃ q
f2d :: Form
f2d = Bin Imp (Bin And (P "p") (Bin Imp (P "p") (P "q"))) (P "q")

-- e. neg(p /\ neg p)
f2e :: Form
f2e = Neg (Bin And (P "p") (Neg (P "p")))

-- Formulas auxiliares
fp :: Form
fp = P "p"

fnn :: Form
fnn = Neg (Neg (P "p"))

-- ============================================
-- Ejercicio 5: Funciones sobre formulas
-- ============================================

-- 5.1 Cantidad de conectivas unarias (negaciones)
cantUnarias :: Form -> Int
cantUnarias (P _) = 0
cantUnarias (Neg f) = 1 + cantUnarias f
cantUnarias (Bin _ f1 f2) = cantUnarias f1 + cantUnarias f2

-- 5.2 Cantidad de conectivas binarias
cantBinarias :: Form -> Int
cantBinarias (P _) = 0
cantBinarias (Neg f) = cantBinarias f
cantBinarias (Bin _ f1 f2) = 1 + cantBinarias f1 + cantBinarias f2


-- 5.3 Cantidad de conectivas cualesquiera
cantConectivas :: Form -> Int
cantConectivas f = cantUnarias f + cantBinarias f


-- 5.4 Cantidad de (apariciones de) letras proposicionales
cantProps :: Form -> Int
cantProps (P _) = 1
cantProps (Neg f) = cantProps f
cantProps (Bin _ f1 f2) = cantProps f1 + cantProps f2

-- 5.5 Cantidad de conectivas y letras proposicionales (= cantidad de subformulas)
cantSubformulas :: Form -> Int
cantSubformulas (P _) = 1
cantSubformulas (Neg f) = 1 + cantSubformulas f
cantSubformulas (Bin _ f1 f2) = 1 + cantSubformulas f1 + cantSubformulas f2

-- 5.6 Retornar todas las subformulas
subformulas :: Form -> [Form]
subformulas (P p) = [P p]
subformulas (Neg f) = Neg f : subformulas f --la fórmula completa, más lo que salga de adentro
subformulas (Bin c f1 f2) = Bin c f1 f2 : subformulas f1 ++ subformulas f2

-- 5.7 Reemplazar alpha ⊃ beta por neg alpha \/ beta
elimImp :: Form -> Form
elimImp (P p) = P p
elimImp (Neg f) = Neg (elimImp f)
elimImp (Bin Imp f1 f2) = Bin Or (Neg (elimImp f1)(elimImp f2))
elimImp (Bin bc f1 f2) = Bin bc (elimImp f1) (elimImp f2)

-- 5.8 Sustitucion simple: alpha[p:=beta]
sust :: Form -> Prop -> Form -> Form
sust 
