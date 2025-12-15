-- modulo de compilacion Just-In-Time (JIT)
-- decide cuando reemplazar una funcion lenta por su version optimizada

module JIT where

import Runtime
import qualified Data.Map as Map

-- numero mínimo de ejecuciones antes de optimizar una funcion
umbralOptimizacion :: Int
umbralOptimizacion = 5

-- ejecuta una funcion, usando la version lenta hasta alcanzar el umbral,
-- luego usa la version optimizada y la almacena en el entorno
ejecutarFuncionConJIT :: Nombre           
                      -> Funcion          -- version lenta (no optimizada)
                      -> Funcion          -- version rapida (optimizada)
                      -> [Int]            -- argumentos de la funcion
                      -> Runtime          
                      -> (Int, Runtime)   -- resultado y nuevo entorno

ejecutarFuncionConJIT nombre lenta rapida args rt =
  let rtConContador = incrementarContador nombre rt
      veces = Map.findWithDefault 0 nombre (contadores rtConContador)
  in
    if veces >= umbralOptimizacion
      then
        let nuevasOptimizadas = Map.insert nombre rapida (optimizadas rtConContador)
        in ( rapida args
           , rtConContador { optimizadas = nuevasOptimizadas }
           )
      else
        ( lenta args, rtConContador )