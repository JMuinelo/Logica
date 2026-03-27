-- docker run --rm --volume "$(pwd)/:/myFiles" -it haskell
-- :l myFiles/bool.hs

module Repaso where

import Prelude (Integer, Show, undefined, (+), (>), (++), otherwise)

--data Bool where {False :: Bool; True :: Bool}
data Bool = False | True 
    deriving Show

nott :: Bool -> Bool
nott = \b -> case b of {True -> False; False -> True}

not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
(&&) = undefined

(||) :: Bool -> Bool -> Bool
(||) = undefined

esPositivo :: Integer -> Bool
esPositivo = undefined

maximo :: Integer -> Integer -> Integer
maximo = undefined

duplicar :: Integer -> Integer
duplicar = undefined

-- data Arb a where {Hoja :: a -> Arb a; Nodo :: Arb a -> a -> Arb a -> Arb a}
data Arb a = Hoja a | Nodo (Arb a) a (Arb a) deriving Show

cantNodoss :: Arb a -> Integer
cantNodoss = \a -> case a of {
                    Hoja x -> 0;
                    Nodo i x d -> 1 + (cantNodoss i) + (cantNodoss d)
                    }

cantNodos :: Arb a -> Integer
cantNodos = undefined

cantHojas :: Arb a -> Integer
cantHojas = undefined

cantA :: Arb a -> Integer
cantA = undefined

listA :: Arb a -> [a]
listA = undefined

mapF :: (a -> b) -> Arb a -> Arb b
mapF = undefined

-- ============================================
-- Arboles de prueba
-- ============================================

--       2
--      / \
--     1   3
t1 :: Arb Integer
t1 = Nodo (Hoja 1) 2 (Hoja 3)

--         5
--        / \
--       2   7
--      / \
--     1   3
t2 :: Arb Integer
t2 = Nodo (Nodo (Hoja 1) 2 (Hoja 3)) 5 (Hoja 7)

t3 :: Arb Integer
t3 = Hoja 42