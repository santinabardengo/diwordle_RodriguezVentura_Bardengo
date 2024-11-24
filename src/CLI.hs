{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CLI (main) where

import Control.Exception (try)
import Core
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Char
import Data.List (elem)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.IO (readFile)
import System.Random.Stateful
import TinyApp.Interactive
  ( ContinueExit (Continue, Exit),
    Event (Key),
    Key (KBS, KChar, KEnter, KEsc),
    Sandbox (..),
    runInteractive,
    runInteractive',
  )
import Wordle
import Wordle (crearJuegoConIntentos)

data State = State
  { juego :: Juego,
    palabraIngresada :: String,
    intentosPreviosProcesados :: Intentos,
    intentosPrevios :: [String],
    mensaje :: Maybe String,
    letrasDescartadas :: String
  }

data GameState = GameState
  { dia :: String, -- Fecha del juego
    palabra :: String, -- Palabra objetivo
    intentosRestantes :: Int, -- Intentos disponibles
    intentosRealizados :: [String] -- Intentos realizados
  }
  deriving (Show, Generic)

data ModoValor = Dia Day | Palabra String

instance FromJSON GameState

instance ToJSON GameState

cargarDiccionario :: IO [[Char]]
cargarDiccionario = do
  contenido <- System.IO.readFile "diccionario.txt"
  let palabras = map (map toUpper . filter isAlpha) (lines contenido)
  return palabras

currentDay :: IO Day
currentDay = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ localDay (utcToLocalTime tz t)

parseDay :: String -> Maybe Day
parseDay s = iso8601ParseM s

cargarEstado :: IO (Maybe GameState)
cargarEstado = do
  -- Intentamos leer el archivo "estado.json"
  contenido <- try (Data.ByteString.Lazy.readFile "estado.json") :: IO (Either IOError Data.ByteString.Lazy.ByteString)
  case contenido of
    Left _ -> return Nothing -- Si ocurre un error (archivo no encontrado), devolvemos Nothing
    Right dataContent -> return (decode dataContent) -- Si se lee el archivo, intentamos decodificarlo

seleccionarPalabraAleatoria :: [[Char]] -> IO String
seleccionarPalabraAleatoria palabras = do
  let (lower, upper) = (0, length palabras - 1)
  idx <- uniformRM (lower, upper) globalStdGen :: IO Int
  return (palabras !! idx)

main :: IO ()
main = do
  args <- getArgs
  palabras <- cargarDiccionario
  diaActual <- currentDay
  -- Utilizamos un Maybe modo por si se ingresa un modo no valido.
  let modo = case args of
        ("--random" : _) -> ("random", Nothing)
        ("--daily" : []) -> ("daily", Just (Dia diaActual))
        ("--daily" : fecha : _) -> ("daily", fmap Dia (parseDay fecha))
        ("--palabra" : palabraFija : _) -> ("palabra", Just (Palabra (map toUpper palabraFija)))
        _ -> ("daily", Just (Dia diaActual))

  case modo of
    ("random", _) -> iniciarJuegoAleatorio palabras
    ("daily", Just (Dia dia)) -> iniciarJuegoDaily dia palabras
    ("palabra", Just (Palabra palabraFija)) -> iniciarJuegoFijo palabraFija palabras
    ("daily", Nothing) -> putStrLn "Fecha inválida."
    _ -> putStrLn "Modo no válido. Usa --random, --daily [YYYY-MM-DD], o --palabra [palabra]"

iniciarJuegoAleatorio :: [[Char]] -> IO ()
iniciarJuegoAleatorio palabras = do
  palabra <- seleccionarPalabraAleatoria palabras
  let juegoInicial = crearJuego palabra 6 (`elem` palabras)
  runInteractive (wordleApp juegoInicial [])

iniciarJuegoDaily :: Day -> [String] -> IO ()
iniciarJuegoDaily dia palabras = do
  estadoGuardado <- cargarEstado
  let diaStr = show dia
  case estadoGuardado of
    Just
      (GameState diaGuardado palabraGuardada intentosRestantes intentosRealizadosNoProcesados)
        | diaGuardado == diaStr -> do
            putStrLn $ "Recuperando el estado del día: " ++ diaStr
            let intentosRealizadosProcesados = procesarMuchos palabraGuardada intentosRealizadosNoProcesados
            let juegoConIntentos = crearJuegoConIntentos palabraGuardada 6 intentosRestantes intentosRealizadosProcesados (`elem` palabras)
            iniciarJuegoConEstadoDaily juegoConIntentos palabras intentosRealizadosNoProcesados diaStr
        | otherwise -> do
            -- Caso en el que la fecha ingresada es valida pero no hay una jugada guardada en dicha fecha
            palabra <- seleccionarPalabraAleatoria palabras
            guardarEstado (GameState diaStr palabra 6 [])
            iniciarJuegoSinEstadoDaily palabra palabras 6 diaStr
    Nothing -> do
      palabra <- seleccionarPalabraAleatoria palabras
      guardarEstado (GameState diaStr palabra 6 [])
      iniciarJuegoSinEstadoDaily palabra palabras 6 diaStr

iniciarJuegoFijo :: String -> [[Char]] -> IO ()
iniciarJuegoFijo palabra palabras = do
  let juegoInicial = crearJuego palabra 6 (`elem` palabras)
  runInteractive (wordleApp juegoInicial [])

iniciarJuegoSinEstadoDaily :: String -> [[Char]] -> Int -> String -> IO ()
iniciarJuegoSinEstadoDaily palabra palabras intentosDisponibles dia = do
  let juegoInicial = crearJuego palabra intentosDisponibles (`elem` palabras)
  -- Ejecutamos el juego en modo interactivo
  s <- runInteractive' (wordleApp juegoInicial [])
  -- Al terminar, guardamos el estado
  let juegoActual = s.juego
  let dfinal = GameState {dia = dia, palabra = palabra, intentosRestantes = obtenerIntentosDisp (juegoActual), intentosRealizados = intentosPrevios s}
  guardarEstado dfinal
  pure ()

iniciarJuegoConEstadoDaily :: Juego -> [[Char]] -> [String] -> String -> IO ()
iniciarJuegoConEstadoDaily juego palabras intentosPrev dia = do
  -- Ejecutamos el juego en modo interactivo
  s <- runInteractive' (wordleApp juego intentosPrev)
  -- Al terminar, guardamos el estado
  let juegoActual = s.juego
  let dfinal = GameState {dia = dia, palabra = obtenerPalabraObjetivo (juegoActual), intentosRestantes = obtenerIntentosDisp (juegoActual), intentosRealizados = intentosPrevios s}
  guardarEstado dfinal
  pure ()

guardarEstado :: GameState -> IO ()
guardarEstado estado = do
  Data.ByteString.Lazy.writeFile "estado.json" (encode estado)

wordleApp :: Juego -> [String] -> Sandbox State
wordleApp juegoInicial intPrev =
  Sandbox
    { initialize = State {juego = juegoInicial, palabraIngresada = "", intentosPreviosProcesados = obtenerIntentos (juegoInicial), intentosPrevios = intPrev, mensaje = Nothing, letrasDescartadas = ""},
      render = \s ->
        let intentosActuales = showIntentos (intentosPreviosProcesados s)
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
      update = \(Key key _) s -> actualizarEstado key s
    }

actualizarEstado :: Key -> State -> (State, ContinueExit)
actualizarEstado key s = case key of
  KEsc -> (s, Exit)
  KEnter -> (procesarIntento s, Continue)
  KBS -> (borrarUltimaLetra s, Continue)
  KChar c
    | isAlpha c && (mensaje s /= Just "Ganaste!" && mensaje s /= Just "Te quedaste sin turnos :(") ->
        if toUpper c `elem` letrasDescartadas s
          then (ingresarLetraInvalida c s, Continue)
          else (ingresarLetra c s, Continue)
    | otherwise -> (s, Continue)
  _ -> (s, Continue)

showIntento :: [(Char, Match)] -> String
showIntento intento = concatMap mostrarResultado intento
  where
    mostrarResultado (c, Correcto) = "\x1b[42m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Verde
    mostrarResultado (c, LugarIncorrecto) = "\x1b[43m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Amarillo
    mostrarResultado (c, NoPertenece) = "\x1b[41m" ++ "|" ++ [c] ++ "|" ++ "\x1b[0m" -- Rojo

showIntentos :: Intentos -> String
showIntentos intentos = unlines (map showIntento intentos)

procesarMuchos :: String -> [String] -> Intentos -- No hay que evaluar el intento porque ya sabemos que fue valido si se realizó en otro juego
procesarMuchos _ [] = []
procesarMuchos objetivo (x : xs) = (match objetivo x) : procesarMuchos objetivo xs

procesarIntento :: State -> State
procesarIntento estado =
  let intento = palabraIngresada estado
   in case enviarIntento intento (juego estado) of
        (Adivino, j) ->
          State j "" (obtenerIntentos j) (intentosPrevios estado ++ [intento]) (Just "¡Ganaste!") (letrasDescartadas estado) -- Si adivinan, reiniciamos el juego
        (NoAdivino, j) ->
          State j "" (obtenerIntentos j) (intentosPrevios estado ++ [intento]) (Just ("Te quedaste sin turnos! La palabra correcta es " ++ obtenerPalabraObjetivo j)) (letrasDescartadas estado) -- Si se quedó sin turnos
        (EnProgresoV, j) ->
          let nuevasLetrasDescartadas = agregarLetrasDescartadas ((obtenerIntentos j) !! (obtenerIntentosTotales j - obtenerIntentosDisp j - 1)) (letrasDescartadas estado)
           in State j "" (obtenerIntentos j) (intentosPrevios estado ++ [intento]) Nothing nuevasLetrasDescartadas -- Actualizamos las letras descartadas
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

agregarLetrasDescartadas :: [(Char, Match)] -> String -> String
agregarLetrasDescartadas [] descartadas = descartadas
agregarLetrasDescartadas ((c, m) : intentos) descartadas
  | m == NoPertenece && notElem c descartadas = agregarLetrasDescartadas intentos (descartadas ++ [c])
  | otherwise = agregarLetrasDescartadas intentos descartadas
