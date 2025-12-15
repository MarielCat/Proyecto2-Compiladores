-- modulo que define el entorno de ejecucion para el compilador JIT
-- mantiene contadores de ejecucion y funciones optimizadas

module Runtime where

import qualified Data.Map as Map

type Nombre   = String   -- nombre de una funcion
type Contador = Int      -- veces que se ha ejecutado una funcion
type Funcion  = [Int] -> Int  -- tipo para funciones que operan sobre listas de enteros

-- representa el estado del entorno de ejecucion
data Runtime = Runtime
  { contadores  :: Map.Map Nombre Contador  -- cuantas veces se ha llamado cada funcion
  , optimizadas :: Map.Map Nombre Funcion   -- funciones ya optimizadas
  }

-- entorno de ejecucion inicial (vacio)
runtimeInicial :: Runtime
runtimeInicial = Runtime Map.empty Map.empty

-- incrementa el contador de ejecuciones para una funcion especifica
incrementarContador :: Nombre -> Runtime -> Runtime
incrementarContador nombre rt =
  let contadorActual = Map.findWithDefault 0 nombre (contadores rt)
      nuevosContadores = Map.insert nombre (contadorActual + 1) (contadores rt)
  in rt { contadores = nuevosContadores }