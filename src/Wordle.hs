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

-- Explicación de estados:
-- EnProgresoNV: indica que el juego esta  en progreso pero que el intento no es válido
-- EnProgresoNoEsta: indica que el juego esta en progreso pero que la palabra no pertenece al diccionario
-- EnProgresoV: indica que el juego esta en progreso y que el intento es válido
-- Adivino: indica que el juego terminó porque el jugador adivinó la palabra
-- NoAdivino: indica que el juego terminó porque el jugador no adivinó y se quedó sin intentos.

data ResultadoValidacion = OK | LongitudInvalida | CharInvalido | NoEsta
  deriving (Show, Eq)

-- Función para crear un juego nuevo con la plabra objetivo, el número de intentos totales y disponibles y un predicado que indica si una palabra esta en el diccionario
crearJuego :: String -> Int -> (String -> Bool) -> Juego
crearJuego objetivoJuego intentosTotalesJuego f =
  Juego
    { objetivo = objetivoJuego,
      intentosTotales = intentosTotalesJuego,
      intentosDisp = intentosTotalesJuego,
      intentos = replicate intentosTotalesJuego [],
      predicado = f
    }

-- Función que replica un juego que ya ha sido jugado anteriormente cargando los intentos previos y simulando las jugadas hasta llegar al estado actual
crearJuegoConIntentos :: String -> Int -> [String] -> (String -> Bool) -> Juego
crearJuegoConIntentos objetivoJuego intentosTotalesJuego intentosGuardados f =
  let juegoInicial = crearJuego objetivoJuego intentosTotalesJuego f
   in foldl (\j i -> snd (enviarIntento i j)) juegoInicial intentosGuardados

tieneTilde :: String -> Bool
tieneTilde intento = any (`elem` "áéíóúÁÉÍÓÚ") intento

validarInput :: String -> Juego -> ResultadoValidacion
validarInput input j
  | length input /= obtenerLongitudObjetivo j = LongitudInvalida
  | tieneTilde input || not (all isUpper input) = CharInvalido
  | not (predicado j input) = NoEsta
  | otherwise = OK

-- Función que dado un intento y un juego devuelve una tupla indicando el estado del juego y el juego resultante
enviarIntento :: String -> Juego -> (Estado, Juego)
enviarIntento intento j
  | (obtenerIntentosDisp j) == 0 = (NoAdivino, j {intentosDisp = (obtenerIntentosDisp j)}) -- Si no tiene más intentos, el juego se termina
  | (validarInput intento j) == NoEsta = (EnProgresoNoEsta, j) -- Si la palabra no está en el diccionario, el juego no se actualiza porque no se procesa el intento
  | (validarInput intento j) /= OK = (EnProgresoNV, j) -- Si el input no es valido, el juego tampoco será modificado
  | intento == objetivo j = (Adivino, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento}) -- Si el intento es igual al objetivo, el juego se termina
  | otherwise = (EnProgresoV, j {intentosDisp = (obtenerIntentosDisp j) - 1, intentos = actualizarPosicion j intento}) -- Si el intento es válido y el jugador sigue teniendo turnos, el juego sigue y se actualiza

-- Función auxiliar que dado un juego y un intento procesa dicho intento en el indice correspondiente segun el número de intento del juego
actualizarPosicion :: Juego -> String -> Intentos
actualizarPosicion j intento = [if i == (obtenerIntentosTotales j - obtenerIntentosDisp j) then match (objetivo j) intento else (intentos j) !! i | i <- [0 .. obtenerIntentosTotales j - obtenerIntentosDisp j]]

-- Getters para no exportar constructores
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