module Core (Match (..), match) where

--- Creamos un tipo de datos Match que va a decidir el estado del caracter del intento con respecto al objetivo
data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

--- Funcion que asigna, a cada caracter del intento, un elemento del tipo de datos Match, segun corresponda
match :: String -> String -> [(Char, Match)]
match objetivo intento = [(intento !! i, mapearIgualdad objetivo intento i) | i <- [0 .. length intento - 1]]

--- Funcion auxiliar que toma una palabra y un caracter y devuelve la cantidad de apariciones de dicho caracer en el input
cantOcurrencias :: String -> Char -> Int
cantOcurrencias "" _ = 0
cantOcurrencias (x : xs) c =
  if x == c
    then
      1 + cantOcurrencias xs c
    else
      cantOcurrencias xs c

--- Funcion auxiliar que, dado un caracter y dos palabras, cuenta la cantidad de veces que dicho caracter coincide en las palabras de input
cantCoincidencias :: Char -> String -> String -> Int
cantCoincidencias _ "" _ = 0
cantCoincidencias _ _ "" = 0
cantCoincidencias c (i : is) (o : os) =
  if c == i && c == o
    then
      1 + cantCoincidencias c is os
    else
      cantCoincidencias c is os

--- Funcion auxiliar que mapea para un caracter en el indice de una palabra, el tipo correcto de Match con respecto a otra palabra
mapearIgualdad :: String -> String -> Int -> Match
mapearIgualdad objetivo intento indice
  | (objetivo !! indice) == c = Correcto --- En el caso de que el caracter del indice indicado en ambas palabras coincide
  | (c `elem` objetivo) && (cantOcurrencias objetivo c >= cantOcurrencias (take (indice + 1) intento) c) && ((cantCoincidencias c objetivo intento) < (cantOcurrencias objetivo c)) = LugarIncorrecto --- Caso en el que el caracter en el indice del intento pertenece al objetivo pero no esta en el lugar correcto
  --- se chequea que el caracter este en el objetivo, que la cantidad de ocurrencias del caracter del objetivo sea mayor a la cantidad de ocurrencias procesadas hasta ahora en el intento (en caso contrario deberia asignarse a NoPertenece) y finalmente que la cantidad de coincidencias del caracter sea menor a la cantidad de ocurrencias de este en el objetivo (caso contrario deberia ser asignado a NoPertenece)
  | otherwise = NoPertenece -- Caso contrario, el caracter no pertenece a la palabra objetivo
  where
    c = intento !! indice

-- Ejemplo
--- >>> match "CASAS" "ACASS"
-- [('A',LugarIncorrecto),('C',LugarIncorrecto),('A',LugarIncorrecto),('S',LugarIncorrecto),('S',Correcto)]
