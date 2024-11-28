{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CLI (main) where

import Control.Exception (try)
import Core
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Char
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

-- Estado que va a ser renderizado en la interfaz
data State = State
  { juego :: Juego,
    palabraIngresada :: String,
    intentosPrevios :: [String],
    mensaje :: Maybe String,
    generador :: StdGen
  }

-- Datos que van a ser guardado en el archivo JSON
data GameState = GameState
  { dia :: String, -- Fecha del juego
    palabra :: String, -- Palabra objetivo
    intentosRestantes :: Int, -- Intentos disponibles
    intentosRealizados :: [String] -- Intentos realizados
  }
  deriving (Show, Generic)

-- El valor que se ingresa como argumento puede ser un tipo Dia o una palabra
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
  gen <- initStdGen
  args <- getArgs
  palabras <- cargarDiccionario
  diaActual <- currentDay
  -- Utilizamos un Maybe modo por si se ingresa un modo no valido.
  let modo = case args of
        ("--random" : _) -> ("random", Nothing)
        ("--daily" : []) -> ("daily", Just (Dia diaActual))
        ("--daily" : fecha : _) ->
          case parseDay fecha of
            Just dia -> ("daily", Just (Dia dia))
            Nothing -> error "Formato de fecha inválido. Usa YYYY-MM-DD."
        ("--palabra" : palabraFija : _) -> ("palabra", Just (Palabra (map toUpper palabraFija)))
        _ -> ("daily", Just (Dia diaActual))

  case modo of
    ("random", _) -> iniciarJuegoAleatorio palabras gen
    ("daily", Just (Dia dia)) -> iniciarJuegoDaily dia palabras gen
    ("palabra", Just (Palabra palabraFija)) -> iniciarJuegoFijo palabraFija palabras gen
    ("daily", Nothing) -> putStrLn "Fecha inválida."
    _ -> putStrLn "Modo no válido. Usa --random, --daily [YYYY-MM-DD], o --palabra [palabra]"

iniciarJuegoAleatorio :: [[Char]] -> StdGen -> IO ()
iniciarJuegoAleatorio palabras gen = do
  palabra <- seleccionarPalabraAleatoria palabras
  let juegoInicial = crearJuego palabra 6 (`elem` palabras)
  runInteractive (wordleApp juegoInicial [] gen)

iniciarJuegoDaily :: Day -> [String] -> StdGen -> IO ()
iniciarJuegoDaily dia palabras gen = do
  estadoGuardado <- cargarEstado
  diaActual <- currentDay
  let diaActualStr = show diaActual
  let diaStr = show dia
  case estadoGuardado of
    Just
      (GameState diaGuardado palabraGuardada _ intentosRealizadosNoProcesados)
        | diaGuardado == diaStr -> do
            -- Caso en el que existe un archivo con el día ingresado, entoces se recupera
            putStrLn $ "Recuperando el estado del día: " ++ diaStr
            let juegoConIntentos = crearJuegoConIntentos palabraGuardada 6 intentosRealizadosNoProcesados (`elem` palabras)
            iniciarJuegoConEstadoDaily juegoConIntentos intentosRealizadosNoProcesados diaActualStr gen
        | otherwise -> do
            -- Caso en el que la fecha ingresada es valida pero no hay una jugada guardada en dicha fecha
            palabra <- seleccionarPalabraAleatoria palabras
            guardarEstado (GameState diaStr palabra 6 [])
            iniciarJuegoSinEstadoDaily palabra palabras 6 diaStr gen
    Nothing -> do
      -- Caso en el que no exista un archivo
      palabra <- seleccionarPalabraAleatoria palabras
      guardarEstado (GameState diaStr palabra 6 [])
      iniciarJuegoSinEstadoDaily palabra palabras 6 diaStr gen

iniciarJuegoFijo :: String -> [[Char]] -> StdGen -> IO ()
iniciarJuegoFijo palabra palabras gen = do
  if palabra `elem` palabras
    then
      let juegoInicial = crearJuego palabra 6 (`elem` palabras)
       in runInteractive (wordleApp juegoInicial [] gen)
    else
      putStr "La palabra ingresada no es válida\n"

-- Función que se utiliza para iniciar un juego en modo daily del que no hay historia previa
iniciarJuegoSinEstadoDaily :: String -> [[Char]] -> Int -> String -> StdGen -> IO ()
iniciarJuegoSinEstadoDaily palabra palabras intentosDisponibles dia gen = do
  let juegoInicial = crearJuego palabra intentosDisponibles (`elem` palabras)
  s <- runInteractive' (wordleApp juegoInicial [] gen)
  -- Al terminar, guardamos el estado de esa jugada
  let juegoActual = s.juego
  let dfinal = GameState {dia = dia, palabra = palabra, intentosRestantes = obtenerIntentosDisp (juegoActual), intentosRealizados = intentosPrevios s}
  guardarEstado dfinal
  pure ()

-- Función que se utiliza para iniciar un juego en modo daily del que sí hay historia previa
iniciarJuegoConEstadoDaily :: Juego -> [String] -> String -> StdGen -> IO ()
iniciarJuegoConEstadoDaily juego intentosPrev dia gen = do
  s <- runInteractive' (wordleApp juego intentosPrev gen)
  -- Al terminar, guardamos el estado
  let juegoActual = s.juego
  let dfinal = GameState {dia = dia, palabra = obtenerPalabraObjetivo (juegoActual), intentosRestantes = obtenerIntentosDisp (juegoActual), intentosRealizados = intentosPrevios s}
  guardarEstado dfinal
  pure ()

guardarEstado :: GameState -> IO ()
guardarEstado estado = do
  Data.ByteString.Lazy.writeFile "estado.json" (encode estado)

-- Lógica de la interfaz que se muestra
wordleApp :: Juego -> [String] -> StdGen -> Sandbox State
wordleApp juegoInicial intPrev g =
  Sandbox
    { initialize = State {juego = juegoInicial, palabraIngresada = "", intentosPrevios = intPrev, mensaje = Nothing, generador = g},
      render = \s ->
        let intentosActuales = showIntentos (obtenerIntentos (juego s))
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
              ++ calcularLetrasDescartadas (obtenerIntentos s.juego) (obtenerPalabraObjetivo s.juego),
      update = \(Key key _) s -> actualizarEstado key s
    }

-- Función auxiliar que contiene la lógica del update de la interfaz
actualizarEstado :: Key -> State -> (State, ContinueExit)
actualizarEstado key s = case key of
  KEsc -> (s, Exit)
  KEnter -> (procesarIntento s, Continue)
  KBS -> (borrarUltimaLetra s, Continue)
  KChar c
    | c == ' ' -> (elegirRandom s (faltantes (obtenerIntentos (juego s)) (obtenerPalabraObjetivo (juego s))), Continue)
    | isAlpha c && (mensaje s /= Just "Ganaste!") && (mensaje s /= Just ("Te quedaste sin turnos! La palabra correcta es " ++ obtenerPalabraObjetivo (juego s))) ->
        if toUpper c `elem` calcularLetrasDescartadas (obtenerIntentos s.juego) (obtenerPalabraObjetivo s.juego)
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

procesarIntento :: State -> State
procesarIntento estado =
  let intento = palabraIngresada estado
   in case enviarIntento intento (juego estado) of
        (Adivino, j) ->
          State j "" (intentosPrevios estado ++ [intento]) (Just "¡Ganaste!") (generador estado) -- Si adivinan, reiniciamos el juego
        (NoAdivino, j) ->
          State j "" (intentosPrevios estado ++ [intento]) (Just ("Te quedaste sin turnos! La palabra correcta es " ++ obtenerPalabraObjetivo j)) (generador estado) --- Si se quedó sin turnos
        (EnProgresoV, j) ->
          State j "" (intentosPrevios estado ++ [intento]) Nothing (generador estado) -- Actualizamos las letras descartadas
        (EnProgresoNV, _) -> estado {mensaje = Just "La palabra tiene caracteres inválidos"} -- Si el intento tiene caracteres inválidos
        (EnProgresoNoEsta, _) -> estado {mensaje = Just "La palabra no es válida"} -- Si la palabra no está en el diccionario

ingresarLetra :: Char -> State -> State
ingresarLetra c estado =
  estado {palabraIngresada = palabraIngresada estado ++ [toUpper c], mensaje = Nothing}

ingresarLetraInvalida :: Char -> State -> State
ingresarLetraInvalida c estado =
  estado {palabraIngresada = palabraIngresada estado ++ [toUpper c], mensaje = Just "Esa letra ya fue descartada"}

borrarUltimaLetra :: State -> State
borrarUltimaLetra estado =
  estado {palabraIngresada = init (palabraIngresada estado)}

calcularLetrasDescartadas :: Intentos -> String -> String
calcularLetrasDescartadas [] _ = ""
calcularLetrasDescartadas (intento : intentos) objetivo = eliminarDuplicadas (calcularLetrasDescartadasIntento intento objetivo ++ calcularLetrasDescartadas intentos objetivo)

calcularLetrasDescartadasIntento :: [(Char, Match)] -> String -> String
calcularLetrasDescartadasIntento [] _ = []
calcularLetrasDescartadasIntento ((c, m) : matches) objetivo
  | m == NoPertenece && notElem c objetivo = c : calcularLetrasDescartadasIntento matches objetivo
  | otherwise = calcularLetrasDescartadasIntento matches objetivo

eliminarDuplicadas :: String -> String
eliminarDuplicadas "" = ""
eliminarDuplicadas (d : ds)
  | d `elem` ds = eliminarDuplicadas ds
  | otherwise = d : eliminarDuplicadas ds

-- A01 Hint
letrasCorrectas :: Intentos -> String
letrasCorrectas [] = ""
letrasCorrectas (intento : intentos) = letrasCorrectasIntento intento ++ letrasCorrectas intentos

letrasCorrectasIntento :: [(Char, Match)] -> String
letrasCorrectasIntento [] = ""
letrasCorrectasIntento ((c, m) : matches)
  | m == Correcto = c : letrasCorrectasIntento matches
  | otherwise = letrasCorrectasIntento matches

faltantes :: Intentos -> String -> String
faltantes intentos objetivo =
  let correctas = letrasCorrectas intentos
   in [c | c <- objetivo, c `notElem` correctas]

elegirRandom :: State -> String -> State
elegirRandom s letrasFaltantes =
  let (indiceAleatorio, nuevoGen) = uniformR (0, length letrasFaltantes - 1) (generador s)
      hint = letrasFaltantes !! indiceAleatorio
   in s {mensaje = Just ("Pista: " ++ [hint]), generador = nuevoGen}