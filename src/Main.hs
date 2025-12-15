-- demuestra la ejecucion JIT

module Main where
import qualified Data.Map as Map
import Runtime
import JIT
import Ejemplos

-- punto de entrada del programa
main :: IO ()
main = cicloEjecucion 0 runtimeInicial

-- ejecuta un ciclo de demostració n por un numero fijo de iteraciones
cicloEjecucion :: Int -> Runtime -> IO ()
cicloEjecucion 10 _ = putStrLn "Ejecucion terminada con 10 iteraciones completadas"

cicloEjecucion n rt =
  let (resultado, rtNuevo) =
        ejecutarFuncionConJIT "suma" sumaLenta sumaRapida [1, 2, 3] rt
      
      -- mostrar información sobre el estado actual
      veces = Map.findWithDefault 0 "suma" (contadores rtNuevo)
      optimizada = Map.member "suma" (optimizadas rtNuevo)
      estado = if optimizada then "optimizada" else "no optimizada"
  in do
    putStrLn $ "Iteracion " ++ show n ++ ":"
    putStrLn $ "  Resultado: " ++ show resultado
    putStrLn $ "  Funcion ''suma'' ejecutada " ++ show veces ++ " veces"
    putStrLn $ "  Estado: " ++ estado
    putStrLn ""
    
    -- continuar con la siguiente iteracion
    cicloEjecucion (n + 1) rtNuevo