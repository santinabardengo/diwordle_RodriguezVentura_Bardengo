module Core (Match (..), match) where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

-- >>> match "posta" "seria"

-- [('s',LugarIncorrecto),('e',NoPertenece),('r',NoPertenece),('i',NoPertenece),('a',Correcto)]

match :: String -> String -> [(Char, Match)]
match objetivo intento = [(intento !! i, mapearIgualdad objetivo (intento !! i) i) | i <- [0 .. length (intento) - 1]]

mapearIgualdad :: String -> Char -> Int -> Match
mapearIgualdad palabra carac indice
  | palabra !! indice == carac = Correcto
  | any (== carac) palabra = LugarIncorrecto
  | otherwise = NoPertenece
