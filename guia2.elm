module Guia2 exposing (..)

{- Guía de ejercicios implementada en Elm -}

-- Ejercicio 1: Búsqueda Genérica
buscar : List Int -> (Int -> Int -> Bool) -> Int
buscar list cmp =
    case list of
        [] ->
            0

        x :: xs ->
            List.foldl (\elem acc -> if cmp elem acc then elem else acc) x xs


-- Ejercicio 2: Máximo y Mínimo
max : List Int -> Int
max list =
    buscar list (\a b -> a > b)


min : List Int -> Int
min list =
    buscar list (\a b -> a < b)


-- Ejercicio 3: Filtros por Umbral
maximos : List Int -> Int -> List Int
maximos list threshold =
    List.filter (\x -> x > threshold) list


minimos : List Int -> Int -> List Int
minimos list threshold =
    List.filter (\x -> x < threshold) list


-- Ejercicio 4: QuickSort
quickSort : List Int -> List Int
quickSort list =
    case list of
        [] ->
            []

        pivot :: rest ->
            let
                menores =
                    List.filter (\x -> x <= pivot) rest

                mayores =
                    List.filter (\x -> x > pivot) rest
            in
            quickSort menores ++ (pivot :: quickSort mayores)


-- Ejercicio 5: Acceso por Índice
obtenerElemento : List Int -> Int -> Int
obtenerElemento list idx =
    if idx < 0 then
        0
    else
        case list of
            [] ->
                0

            _ ->
                case List.drop idx list |> List.head of
                    Just v ->
                        v

                    Nothing ->
                        0


-- Ejercicio 6: Mediana
mediana : List Int -> Int
mediana list =
    case list of
        [] ->
            0

        _ ->
            let
                sorted =
                    quickSort list

                n =
                    List.length sorted

                midIndex =
                    if modBy 2 n == 1 then
                        n // 2

                    else
                        (n // 2) - 1
            in
            obtenerElemento sorted midIndex


-- Ejercicio 7: Contar y Acumular
contar : List Int -> Int
contar =
    List.length


acc : List Int -> Int
acc =
    List.sum


-- Ejercicio 8: Filtrado Genérico
filtrar : List Int -> (Int -> Bool) -> List Int
filtrar list pred =
    List.filter pred list


filtrarPares : List Int -> List Int
filtrarPares list =
    filtrar list (\x -> modBy 2 x == 0)


filtrarMultiplosDeTres : List Int -> List Int
filtrarMultiplosDeTres list =
    filtrar list (\x -> modBy 3 x == 0)


-- Ejercicio 9: Acumulación con Transformación
acumular : List Int -> (Int -> Int) -> Int
acumular list f =
    List.map f list |> List.sum


acumularUnidad : List Int -> Int
acumularUnidad list =
    acumular list (\_ -> 1)


acumularDoble : List Int -> Int
acumularDoble list =
    acumular list (\x -> x * 2)


acumularCuadrado : List Int -> Int
acumularCuadrado list =
    acumular list (\x -> x * x)


-- Ejercicio 10: Operaciones con Listas
unir : List Int -> List Int -> List Int
unir a b =
    a ++ b


transformar : List Int -> (Int -> a) -> List a
transformar list f =
    List.map f list


existe : List Int -> Int -> Bool
existe list value =
    List.member value list


-- Ejercicio 11: Unión sin Duplicados
removerDuplicados : List Int -> List Int
removerDuplicados list =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        list


unirOfSet : List Int -> List Int -> List Int
unirOfSet a b =
    removerDuplicados (a ++ b)


-- Ejercicios Opcionales: Subconjuntos
subSets : List Int -> List (List Int)
subSets list =
    List.foldl
        (\x acc -> acc ++ List.map (\s -> s ++ [ x ]) acc)
        [ [] ]
        list


-- Ejercicios Opcionales: Dividir en Grupos
tomar : Int -> List a -> List a
tomar =
    List.take


saltar : Int -> List a -> List a
saltar =
    List.drop


cortar : List Int -> Int -> List (List Int)
cortar list n =
    if n <= 0 then
        []

    else
        case list of
            [] ->
                []

            _ ->
                let
                    head =
                        tomar n list

                    rest =
                        saltar n list
                in
                head :: cortar rest n