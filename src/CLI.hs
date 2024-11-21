{-# LANGUAGE DeriveGeneric #-}

module CLI (main) where

import Core
import Data.Char
import Data.List (elem)
import System.IO (readFile)
import System.Random.Stateful
import TinyApp.Interactive
  ( ContinueExit (Continue, Exit),
    Event (Key),
    Key (KBS, KChar, KEnter, KEsc),
    Sandbox (..),
    runInteractive,
  )
import Wordle
  ( Estado (Adivino, EnProgresoNV, EnProgresoV, NoAdivino),
    Intentos,
    Juego,
    crearJuego,
    enviarIntento,
    obtenerIntentos,
    obtenerIntentosDisp,
  )

data State = State
  { juego :: Juego,
    palabraIngresada :: String,
    intentos :: Intentos,
    mensaje :: Maybe String
  }

-- Cargar el diccionario desde un archivo de texto
cargarDiccionario :: IO ([[Char]])
cargarDiccionario = do
  contenido <- readFile "diccionario.txt"
  let palabras = map (map toUpper . filter isAlpha) (lines contenido)
  return palabras -- Predicado para verificar si la palabra está en el diccionario

main :: IO ()
main = do
  putStrLn "¡Bienvenido a Wordle!"
  putStrLn "Introducí la palabra secreta: "
  palabra <- getLine
  palabras <- cargarDiccionario
  palabraAleatoria <- seleccionarPalabraAleatoria palabras
  let juegoInicial =
        if (palabra == "")
          then
            crearJuego (palabraAleatoria) 6 (\palabra -> palabra `elem` palabras)
          else
            crearJuego (map toUpper palabra) 6 (\palabra -> palabra `elem` palabras)
  runInteractive (wordleApp juegoInicial)

wordleApp :: Juego -> Sandbox State
wordleApp juegoInicial =
  Sandbox
    { initialize = State {juego = juegoInicial, palabraIngresada = "", intentos = [], mensaje = Nothing},
      render = \s ->
        let intentosActuales = showIntentos (intentos s)
            intentosDisponibles = obtenerIntentosDisp (juego s)
            mensajeAMostrar = case mensaje s of
              Just m -> m
              Nothing -> ""
         in "Intentos realizados:\n"
              ++ intentosActuales
              ++ "\n"
              ++ "Palabra ingresada: "
              ++ palabraIngresada s
              ++ "\nIntentos disponibles: "
              ++ show intentosDisponibles
              ++ "\n"
              ++ mensajeAMostrar,
      update = \(Key key _) s ->
        case key of
          KEsc -> (s, Exit)
          KEnter -> (procesarIntento s, Continue)
          KBS -> (borrarUltimaLetra s, Continue)
          KChar ' ' -> (s {juego = juegoInicial, palabraIngresada = "", intentos = [], mensaje = Nothing}, Continue)
          KChar c ->
            if isAlpha c && (mensaje s /= Just "Ganaste!" || mensaje s /= Just "Te quedaste sin turnos :(")
              then (ingresarLetra c s, Continue)
              else (s, Continue)
          _ -> (s, Continue)
    }

showIntento :: [(Char, Match)] -> String
showIntento intento = concat (map mostrarResultado intento)
  where
    mostrarResultado (c, Correcto) = "\x1b[42m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Verde
    mostrarResultado (c, LugarIncorrecto) = "\x1b[43m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Amarillo
    mostrarResultado (c, NoPertenece) = "\x1b[41m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Rojo

showIntentos :: Intentos -> String
showIntentos intentos = unlines (map formatIntento intentos)
  where
    formatIntento intento = showIntento intento

procesarIntento :: State -> State
procesarIntento estado =
  let intento = palabraIngresada estado
   in case enviarIntento intento (juego estado) of
        (Adivino, j) -> State j "" (obtenerIntentos j) (Just "Ganaste!") -- Si adivinan, reiniciamos el juego
        (NoAdivino, j) -> State j "" (obtenerIntentos j) (Just "Te quedaste sin turnos :(") -- Si se quedó sin turnos, no agregamos más intentos.
        (EnProgresoV, j) -> State j "" (obtenerIntentos j) Nothing -- Si el intento es válido, lo agregamos a la lista de intentos. ESTE ESTA MAL YO DEBO HACER QUE INTENTOS ESTADO SEA INTENTOS JUEGO
        (EnProgresoNV, _) -> estado {mensaje = Just "Intento Inválido"} -- Si el intento no es válido, no actualizamos nada.

ingresarLetra :: Char -> State -> State
ingresarLetra c estado =
  estado {palabraIngresada = palabraIngresada estado ++ [toUpper c]}

borrarUltimaLetra :: State -> State
borrarUltimaLetra estado =
  estado {palabraIngresada = init (palabraIngresada estado)}

seleccionarPalabraAleatoria :: [[Char]] -> IO String
seleccionarPalabraAleatoria palabras = do
  let (lower, upper) = (0, length palabras - 1)
  idx <- uniformRM (lower, upper) globalStdGen :: IO Int
  return (palabras !! idx)