module Wordle
  ( Juego,
    Estado (..),
    ResultadoValidacion (..),
    Intentos,
    crearJuego,
    crearJuegoConIntentos,
    validarInput,
    enviarIntento,
    obtenerIntentosDisp,
    obtenerIntentosTotales,
    obtenerLongitudObjetivo,
    obtenerIntentos,
    obtenerPalabraObjetivo,
  )
where

import Core
import Data.Char (isUpper)

data Juego = Juego {objetivo :: String, intentosTotales :: Int, intentosDisp :: Int, intentos :: Intentos, predicado :: String -> Bool}

type Intentos = [[(Char, Match)]]

data Estado = EnProgresoNV | EnProgresoNoEsta | EnProgresoV | Adivino | NoAdivino
  deriving (Show, Eq)

data ResultadoValidacion = OK | LongitudInvalida | CharInvalido | NoEsta
  deriving (Show, Eq)

crearJuego :: String -> Int -> (String -> Bool) -> Juego
crearJuego objetivoJuego intentosTotalesJuego f =
  Juego
    { objetivo = objetivoJuego,
      intentosTotales = intentosTotalesJuego,
      intentosDisp = intentosTotalesJuego,
      intentos = replicate intentosTotalesJuego [],
      predicado = f
    }

crearJuegoConIntentos :: String -> Int -> Int -> Intentos -> (String -> Bool) -> Juego
crearJuegoConIntentos objetivoJuego intentosTotalesJuego intentosRest intentosGuardados f =
  Juego
    { objetivo = objetivoJuego,
      intentosTotales = intentosTotalesJuego,
      intentosDisp = intentosRest,
      intentos = intentosGuardados,
      predicado = f
    }

tieneTilde :: String -> Bool
tieneTilde intento = any (`elem` "áéíóúÁÉÍÓÚ") intento

validarInput :: String -> Juego -> ResultadoValidacion
validarInput input j
  | length input /= obtenerLongitudObjetivo j = LongitudInvalida
  | tieneTilde input || not (all isUpper input) = CharInvalido
  | not (predicado j input) = NoEsta
  | otherwise = OK

enviarIntento :: String -> Juego -> (Estado, Juego)
enviarIntento intento j
  | (obtenerIntentosDisp j) == 0 = (NoAdivino, j {intentosDisp = (obtenerIntentosDisp j)})
  | (validarInput intento j) == NoEsta = (EnProgresoNoEsta, j)
  | (validarInput intento j) /= OK = (EnProgresoNV, j)
  | intento == objetivo j = (Adivino, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento})
  | otherwise = (EnProgresoV, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento})

actualizarPosicion :: Juego -> String -> Intentos
actualizarPosicion j intento = [if i == (obtenerIntentosTotales j - obtenerIntentosDisp j) then match (objetivo j) intento else (intentos j) !! i | i <- [0 .. obtenerIntentosTotales j - obtenerIntentosDisp j]]

obtenerLongitudObjetivo :: Juego -> Int
obtenerLongitudObjetivo j = length (objetivo j)

obtenerIntentosDisp :: Juego -> Int
obtenerIntentosDisp j = intentosDisp j

obtenerIntentosTotales :: Juego -> Int
obtenerIntentosTotales j = intentosTotales j

obtenerIntentos :: Juego -> Intentos
obtenerIntentos j = intentos j

obtenerPalabraObjetivo :: Juego -> String
obtenerPalabraObjetivo j = objetivo j