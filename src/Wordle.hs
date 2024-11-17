module Wordle where

import Core
import Data.Char (isUpper)

data Juego = Juego {objetivo :: String, intentosTotales :: Int, intentosDisp :: Int, intentos :: Intentos}
  deriving (Show, Eq)

type Intentos = [[(Char, Match)]]

data Estado = EnProgresoNV | EnProgresoV | Adivino | NoAdivino
  deriving (Show, Eq)

data ResultadoValidacion = OK | LongitudInvalida | CharInvalido
  deriving (Show, Eq)

crearJuego :: String -> Int -> Juego
crearJuego objetivo intentosTotales =
  Juego
    { objetivo = objetivo,
      intentosTotales = intentosTotales,
      intentosDisp = intentosTotales,
      intentos = replicate intentosTotales []
    }

tieneTilde :: String -> Bool
tieneTilde intento = any (`elem` "áéíóúÁÉÍÓÚ") intento

validarInput :: String -> Juego -> ResultadoValidacion
validarInput input j
  | length (input) /= obtenerLongitudObjetivo j = LongitudInvalida
  | tieneTilde input || not (all isUpper input) = CharInvalido
  | otherwise = OK

enviarIntento :: String -> Juego -> (Estado, Juego)
enviarIntento intento j
  | (obtenerIntentosDisp j) == 0 = (NoAdivino, j)
  | (validarInput intento j) /= OK = (EnProgresoNV, j)
  | intento == objetivo j = (Adivino, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento})
  | otherwise = (EnProgresoV, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento})

actualizarPosicion :: Juego -> String -> Intentos
actualizarPosicion j intento = [if i == (obtenerIntentosTotales j - obtenerIntentosDisp j) then match (objetivo j) intento else (intentos j) !! i | i <- [0 .. obtenerIntentosTotales j - 1]]

obtenerLongitudObjetivo :: Juego -> Int
obtenerLongitudObjetivo j = length (objetivo j)

obtenerIntentosDisp :: Juego -> Int
obtenerIntentosDisp j = intentosDisp j

obtenerIntentosTotales :: Juego -> Int
obtenerIntentosTotales j = intentosTotales j