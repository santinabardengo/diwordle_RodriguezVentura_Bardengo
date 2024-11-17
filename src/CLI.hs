module CLI (main) where

import Data.Char (toUpper)
import TinyApp.Interactive
import Wordle

data State = State
  { juego :: Juego,
    palabraIngresada :: String,
    intentos :: [String]
  }

main :: IO ()
main = do
  putStrLn "¡Bienvenido a Wordle!"
  putStrLn "Introduce la palabra secreta: "
  palabra <- getLine
  let juegoInicial = crearJuego (map toUpper palabra) 6
  let estadoInicial = State juegoInicial "" []
  runInteractive juegoInicial estadoInicial

runInteractive :: Juego -> State -> IO ()
runInteractive juego estado = runSandbox (mainSandbox estado juego)

mainSandbox :: State -> Juego -> Sandbox State
mainSandbox estado juego =
  Sandbox
    { initialize = estado,
      render = renderEstado juego,
      update = actualizarEstado juego
    }

renderEstado :: Juego -> State -> String
renderEstado juego estado =
  let juegoActual = showJuego juego
      intentosActuales = showIntentos (intentos estado)
   in "Palabra secreta de "
        ++ show (obtenerLongitudObjetivo juego)
        ++ " letras.\n"
        ++ "Intentos restantes: "
        ++ show (obtenerIntentosDisp juego)
        ++ "\n\n"
        ++ juegoActual
        ++ "\n\n"
        ++ intentosActuales

showJuego :: Juego -> String
showJuego juego = unlines (map showIntento (intentos juego))

showIntento :: [(Char, Match)] -> String
showIntento intento = concatMap mostrarResultado intento
  where
    mostrarResultado (c, Correcto) = "\x1b[42m" ++ [c] ++ "\x1b[0m" -- Verde
    mostrarResultado (c, LugarIncorrecto) = "\x1b[43m" ++ [c] ++ "\x1b[0m" -- Amarillo
    mostrarResultado (c, NoPertenece) = "\x1b[41m" ++ [c] ++ "\x1b[0m" -- Rojo

showIntentos :: [String] -> String
showIntentos = unlines . map (\i -> "| " ++ i ++ " |")

actualizarEstado :: Juego -> Key -> State -> (State, SandboxAction)
actualizarEstado juego key estado = case key of
  KEsc -> (estado, Exit)
  KEnter -> procesarIntento juego estado
  KBS -> borrarUltimaLetra estado
  KChar c -> ingresarLetra c estado
  _ -> (estado, Continue)

procesarIntento :: Juego -> State -> (State, SandboxAction)
procesarIntento juego estado =
  let intento = palabraIngresada estado
   in case enviarIntento intento juego of
        (Adivino, j) -> (State j "" [], Exit) -- Si adivinó la palabra, termina el juego
        (NoAdivino, j) -> (State j "" (intentos estado ++ [intento]), Continue)
        (EnProgresoV, j) -> (State j intento (intentos estado), Continue)
        (EnProgresoNV, j) -> (estado, Continue)

ingresarLetra :: Char -> State -> (State, SandboxAction)
ingresarLetra c estado = (estado {palabraIngresada = palabraIngresada estado ++ [toUpper c]}, Continue)

borrarUltimaLetra :: State -> (State, SandboxAction)
borrarUltimaLetra estado = (estado {palabraIngresada = init (palabraIngresada estado)}, Continue)
