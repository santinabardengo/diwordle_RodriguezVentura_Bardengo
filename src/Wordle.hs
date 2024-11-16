module Wordle where

import Core
import Data.Char (isUpper)

data Juego = Juego {objetivo :: String, cantIntentos :: Int, intentos :: Intentos}
  deriving (Show, Eq)

data Estado = Terminado | EnProgresoNV | EnProgresoV
  deriving (Show, Eq)

data ResultadoValidacion = OK | LongitudInvalida | CharInvalido
  deriving (Show, Eq)

type Intentos = [[(Char, Match)]]

inicializarIntentos :: Juego -> Intentos
inicializarIntentos j = replicate (cantIntentos j) []

tieneTilde :: String -> Bool
tieneTilde intento = any (`elem` "áéíóúÁÉÍÓÚ") intento

validarInput :: String -> Juego -> ResultadoValidacion
validarInput input j
  | length (input) /= length (objetivo j) = LongitudInvalida
  | tieneTilde input || not (all isUpper input) = CharInvalido
  | otherwise = OK

enviarIntento :: String -> Juego -> (Estado, Juego)
enviarIntento intento j
  | (cantIntentos j) == 0 = (Terminado, j)
  | (validarInput intento j) /= OK = (EnProgresoNV, j)
  | otherwise = (EnProgresoV, j {cantIntentos = (cantIntentos j) - 1, intentos = (intentos j) ++ [match (objetivo j) intento]})
