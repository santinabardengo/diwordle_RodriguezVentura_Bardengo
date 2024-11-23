module Core (Match (..), match) where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

-- >>> match "posta" "seria"

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

mapearIgualdad :: String -> String -> Int -> Match
mapearIgualdad objetivo intento indice
  | (objetivo !! indice) == (intento !! indice) = Correcto
  | ((intento !! indice) `elem` objetivo) && (cantOcurrencias objetivo (intento !! indice) >= cantOcurrencias (take (indice + 1) intento) (intento !! indice)) = LugarIncorrecto
  | otherwise = NoPertenece

--- >>> match "RANGO" "ACASS"
-- [('A',LugarIncorrecto),('C',NoPertenece),('A',NoPertenece),('S',NoPertenece),('S',NoPertenece)]
