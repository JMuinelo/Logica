module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude

-- EJERCICIO 1.1 --
type Var = String

data L = Neg L
       | Bin L BC L
       | V Var
        deriving (Show, Eq)
data BC = Or | And | Imp | Iff
  deriving (Show, Eq)
  
  
-- EJERCICIO 1.2 --
--a)p ∧ ¬¬q
fa :: L
fa = Bin And (V "p")(Neg (Neg (V "q")))
--b) p ∧ ¬ q ∧ ¬ r
fb :: L
fb = Bin And ((V "p"))(Bin And (Neg (V "q")) (Neg (V "r"))) --p ∧ (¬ q ∧ (¬ r))
--c)¬¬ p ∨ ¬ (q ∧ p)
fc :: L
fc = Bin Or (Neg(Neg (V "p"))) (Neg (Bin And (V "q") (V "p")))
--d)¬(r ⊃ r) ∧ (¬¬ p ∨ ¬ (q ∧ p))
fd :: L
fd = Bin And (Neg(Bin Imp (V "r") (V "r"))) (Bin Or (Neg (Neg (V "p"))) (Neg (Bin And (V "q") (V "p"))))


-- EJERCICIO 1.3 --
--a)
cantBin :: L -> Int
cantBin (V _) = 0
cantBin (Neg l) = cantBin l
cantBin (Bin f1 _ f2) = 1 + cantBin f1 + cantBin f2

--b)
valores :: L -> [(Var,Bool)]
valores (V p) = [(p,True)]
valores (Neg (V p)) = [(p,False)]
valores (Bin f1 _ f2)= valores f1 ++ valores f2

--c)
dobleNeg :: L -> L
dobleNeg (V p) = V p
dobleNeg (Neg (Neg f)) = dobleNeg f 
dobleNeg (Neg f) = Neg (dobleNeg f)
dobleNeg (Bin f1 c  f2) =  Bin (dobleNeg f1) c (dobleNeg f2)


--d)
cambiar :: L -> L
cambiar (V p) = V p
cambiar (Neg f) = Neg (cambiar f)
cambiar (Bin Or f1 f2) = Bin Imp (Neg f1) (f2)
cambiar (Bin f1 b  f2)= Bin (cambiar f1) b  f2 

--e)
cantPropX :: L -> Var -> Int
cantPropX (V p1) (p2) 
    |p1 == p2 = 1
    |otherwise = 0
cantPropX (Neg f) (r) = cantPropX(f) (r)
cantPropX(Bin f1 b f2) (q) = cantPropX (f1) (q) + cantPropX (f2) (q)

--f)
listarProp :: L -> [Var]
listarProp (V p) = [p]
listarProp (Neg f) = listarProp f


--g)
sustCon :: L -> BC -> BC -> L
sustCon (V p) b1 b2 = V p
sustCon (Neg f) b1 b2 = Neg (sustCon f b1 b2)
sustCon (Bin f1 c f2) b1 b2
    | c == b1   = Bin (sustCon f1 b1 b2) b2  (sustCon f2 b1 b2)
    | otherwise = Bin (sustCon f1 b1 b2) c (sustCon f2 b1 b2)

--h)
swapCon :: L -> BC -> BC -> L
swapCon (V p) b1 b2 = V p
swapCon (Neg f) b1 b2 = Neg (swapCon f b1 b2)
swapCon (Bin f1 c  f2) b1 b2
    | c == b1   = Bin (swapCon f1 b1 b2) b2  (swapCon f2 b1 b2)
    | c == b2   = Bin (swapCon f1 b1 b2) b1  (swapCon f2 b1 b2)
    | otherwise = Bin (swapCon f1 b1 b2) c   (swapCon f2 b1 b2)

--i)
invertir :: L -> L
invertir (V p) = Neg (V p)
invertir (Neg f) = dobleNeg (Neg (invertir f))
invertir (Bin  f1 c f2) = dobleNeg (swapCon (Bin (invertir f1) c (invertir f1) (invertir f2)) And Or)

--j)
sustSimp :: Var -> L -> L -> L
sustSimp = undefined

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult = undefined