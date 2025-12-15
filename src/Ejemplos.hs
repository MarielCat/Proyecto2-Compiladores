-- modulo con funciones de ejemplo para demostrar la optimizacion JIT

module Ejemplos where

-- versión lenta de suma: usa foldl explicito (menos eficiente)
sumaLenta :: [Int] -> Int
sumaLenta xs = foldl (\acc x -> acc + x) 0 xs

-- version rapida de suma: usa la funcion estandar sum (mas eficiente)
sumaRapida :: [Int] -> Int
sumaRapida = sum