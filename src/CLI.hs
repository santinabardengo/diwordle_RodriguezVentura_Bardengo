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
  ( Estado (Adivino, EnProgresoNV, EnProgresoNoEsta, EnProgresoV, NoAdivino),
    Intentos,
    Juego,
    crearJuego,
    enviarIntento,
    obtenerIntentos,
    obtenerIntentosDisp,
    obtenerIntentosTotales,
    obtenerPalabraObjetivo,
  )

data State = State
  { juego :: Juego,
    palabraIngresada :: String,
    intentos :: Intentos,
    mensaje :: Maybe String,
    letrasDescartadas :: String
  }

cargarDiccionario :: IO ([[Char]])
cargarDiccionario = do
  contenido <- readFile "diccionario.txt"
  let palabras = map (map toUpper . filter isAlpha) (lines contenido)
  return palabras

main :: IO ()
main = do
  putStrLn "¡Bienvenido a Wordle!"
  putStrLn "Introducí la palabra secreta: "
  palabra <- getLine
  palabras <- cargarDiccionario
  palabraAleatoria <- seleccionarPalabraAleatoria palabras
  let juegoInicial =
        if palabra == ""
          then
            crearJuego (palabraAleatoria) 6 (\palabra -> palabra `elem` palabras)
          else
            crearJuego (map toUpper palabra) 6 (\palabra -> palabra `elem` palabras)
  runInteractive (wordleApp juegoInicial)

wordleApp :: Juego -> Sandbox State
wordleApp juegoInicial =
  Sandbox
    { initialize = State {juego = juegoInicial, palabraIngresada = "", intentos = [], mensaje = Nothing, letrasDescartadas = ""},
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
              ++ mensajeAMostrar
              ++ "\n"
              ++ "Letras descartadas:"
              ++ letrasDescartadas s,
      update = \(Key key _) s ->
        case key of
          KEsc -> (s, Exit)
          KEnter -> (procesarIntento s, Continue)
          KBS -> (borrarUltimaLetra s, Continue)
          KChar ' ' -> (s {juego = juegoInicial, palabraIngresada = "", intentos = [], mensaje = Nothing}, Continue)
          KChar c
            | isAlpha c && (mensaje s /= Just "Ganaste!" && mensaje s /= Just "Te quedaste sin turnos :(") ->
                if toUpper c `elem` letrasDescartadas s
                  then (ingresarLetraInvalida c s, Continue)
                  else (ingresarLetra c s, Continue)
            | otherwise -> (s, Continue)
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
        (Adivino, j) ->
          State j "" (obtenerIntentos j) (Just "¡Ganaste!") (letrasDescartadas estado) -- Si adivinan, reiniciamos el juego
        (NoAdivino, j) ->
          State j "" (obtenerIntentos j) (Just ("Te quedaste sin turnos! La palabra correcta es " ++ obtenerPalabraObjetivo j)) (letrasDescartadas estado) -- Si se quedó sin turnos
        (EnProgresoV, j) ->
          let nuevasLetrasDescartadas = agregarLetrasDescartadas ((obtenerIntentos j) !! (obtenerIntentosTotales j - obtenerIntentosDisp j - 1)) (letrasDescartadas estado)
           in State j "" (obtenerIntentos j) Nothing nuevasLetrasDescartadas -- Actualizamos las letras descartadas
        (EnProgresoNV, _) -> estado {mensaje = Just "La palabra tiene caracteres inválidos"} -- Si el intento tiene caracteres inválidos
        (EnProgresoNoEsta, _) -> estado {mensaje = Just "La palabra no es válida"}

ingresarLetra :: Char -> State -> State
ingresarLetra c estado =
  estado {palabraIngresada = palabraIngresada estado ++ [toUpper c], mensaje = Nothing}

ingresarLetraInvalida :: Char -> State -> State
ingresarLetraInvalida c estado =
  estado {palabraIngresada = palabraIngresada estado ++ [toUpper c], mensaje = Just "Esa letra ya fue descartada"}

borrarUltimaLetra :: State -> State
borrarUltimaLetra estado =
  estado {palabraIngresada = init (palabraIngresada estado)}

seleccionarPalabraAleatoria :: [[Char]] -> IO String
seleccionarPalabraAleatoria palabras = do
  let (lower, upper) = (0, length palabras - 1)
  idx <- uniformRM (lower, upper) globalStdGen :: IO Int
  return (palabras !! idx)

agregarLetrasDescartadas :: [(Char, Match)] -> String -> String
agregarLetrasDescartadas [] descartadas = descartadas
agregarLetrasDescartadas ((c, m) : intentos) descartadas
  | m == NoPertenece && notElem c descartadas = agregarLetrasDescartadas intentos (descartadas ++ [c])
  | otherwise = agregarLetrasDescartadas intentos descartadas
