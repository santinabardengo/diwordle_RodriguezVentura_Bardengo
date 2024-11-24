module Core (Match (..), match) where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

-- >>> match "BURIO" "VENDI"
-- [('V',NoPertenece),('E',NoPertenece),('N',NoPertenece),('D',NoPertenece),('I',LugarIncorrecto)]

-- [('s',LugarIncorrecto),('e',NoPertenece),('r',NoPertenece),('i',NoPertenece),('a',Correcto)]

match :: String -> String -> [(Char, Match)]
match objetivo intento = [(intento !! i, mapearIgualdad objetivo intento i) | i <- [0 .. length intento - 1]]

cantOcurrencias :: String -> Char -> Int
cantOcurrencias "" _ = 0
cantOcurrencias (x : xs) c =
  if x == c
    then
      1 + cantOcurrencias xs c
    else
      cantOcurrencias xs c

cantCoincidencias :: Char -> String -> String -> Int
cantCoincidencias _ "" "" = 0
cantCoincidencias c (i : is) (o : os) =
  if c == i && c == o
    then
      1 + cantCoincidencias c is os
    else
      cantCoincidencias c is os

mapearIgualdad :: String -> String -> Int -> Match
mapearIgualdad objetivo intento indice
  | (objetivo !! indice) == (c) = Correcto
  | (c `elem` objetivo) && (cantOcurrencias objetivo (c) >= cantOcurrencias (take (indice + 1) intento) (c)) && ((cantCoincidencias c objetivo intento) < (cantOcurrencias objetivo c)) = LugarIncorrecto
  | otherwise = NoPertenece
  where
    c = intento !! indice

--- >>> match "CASAS" "ACASS"
-- [('A',LugarIncorrecto),('C',LugarIncorrecto),('A',LugarIncorrecto),('S',LugarIncorrecto),('S',Correcto)]
