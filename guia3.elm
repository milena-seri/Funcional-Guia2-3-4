-- filepath: c:\Users\elcha\paradigmaUAP2025\funcional\ejercicios\ejercicio3\guia3.elm
module Guia3 exposing (..)

import String


{- Guía 3 de ejercicios - Programación Funcional en Elm -}


{- ========== PARTE 0: Implementaciones Personalizadas ========== -}


-- Ejercicio 1: Map Personalizado
miMap : (a -> b) -> List a -> List b
miMap f list =
    case list of
        [] ->
            []

        x :: xs ->
            f x :: miMap f xs


-- Ejercicio 2: Filter Personalizado
miFiltro : (a -> Bool) -> List a -> List a
miFiltro pred list =
    case list of
        [] ->
            []

        x :: xs ->
            if pred x then
                x :: miFiltro pred xs

            else
                miFiltro pred xs


-- Ejercicio 3: Foldl Personalizado
miFoldl : (a -> b -> b) -> b -> List a -> b
miFoldl f acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            miFoldl f (f x acc) xs



{- ========== PARTE 1: Entendiendo Map ========== -}


-- Ejercicio 4: Duplicar Números
duplicar : List Int -> List Int
duplicar list =
    List.map (\x -> x * 2) list


-- Ejercicio 5: Longitudes de Strings
longitudes : List String -> List Int
longitudes list =
    List.map String.length list


-- Ejercicio 6: Incrementar Todos
incrementarTodos : List Int -> List Int
incrementarTodos list =
    List.map (\x -> x + 1) list


-- Ejercicio 7: A Mayúsculas
todasMayusculas : List String -> List String
todasMayusculas list =
    List.map String.toUpper list


-- Ejercicio 8: Negar Booleanos
negarTodos : List Bool -> List Bool
negarTodos list =
    List.map not list



{- ========== PARTE 2: Entendiendo Filter ========== -}


-- Ejercicio 9: Números Pares
pares : List Int -> List Int
pares list =
    List.filter (\x -> modBy 2 x == 0) list


-- Ejercicio 10: Números Positivos
positivos : List Int -> List Int
positivos list =
    List.filter (\x -> x > 0) list


-- Ejercicio 11: Strings Largos
stringsLargos : List String -> List String
stringsLargos list =
    List.filter (\s -> String.length s > 5) list


-- Ejercicio 12: Remover Falsos
soloVerdaderos : List Bool -> List Bool
soloVerdaderos list =
    List.filter (\b -> b == True) list


-- Ejercicio 13: Mayor Que
mayoresQue : Int -> List Int -> List Int
mayoresQue threshold list =
    List.filter (\x -> x > threshold) list



{- ========== PARTE 3: Entendiendo Fold ========== -}


-- Ejercicio 14: Suma con Fold
sumaFold : List Int -> Int
sumaFold list =
    List.foldl (+) 0 list


-- Ejercicio 15: Producto
producto : List Int -> Int
producto list =
    List.foldl (*) 1 list


-- Ejercicio 16: Contar con Fold
contarFold : List a -> Int
contarFold list =
    List.foldl (\_ acc -> acc + 1) 0 list


-- Ejercicio 17: Concatenar Strings
concatenar : List String -> String
concatenar list =
    List.foldl (++) "" list


-- Ejercicio 18: Valor Máximo
maximo : List Int -> Int
maximo list =
    case list of
        [] ->
            0

        x :: xs ->
            List.foldl (\elem acc -> if elem > acc then elem else acc) x xs


-- Ejercicio 19: Invertir con Fold
invertirFold : List a -> List a
invertirFold list =
    List.foldl (::) [] list


-- Ejercicio 20: Todos Verdaderos
todos : (a -> Bool) -> List a -> Bool
todos pred list =
    List.foldl (\elem acc -> acc && pred elem) True list


-- Ejercicio 21: Alguno Verdadero
alguno : (a -> Bool) -> List a -> Bool
alguno pred list =
    List.foldl (\elem acc -> acc || pred elem) False list



{- ========== PARTE 4: Combinando Operaciones ========== -}


-- Ejercicio 22: Suma de Cuadrados
sumaDeCuadrados : List Int -> Int
sumaDeCuadrados list =
    list
        |> List.map (\x -> x * x)
        |> List.sum


-- Ejercicio 23: Contar Números Pares
contarPares : List Int -> Int
contarPares list =
    list
        |> List.filter (\x -> modBy 2 x == 0)
        |> List.length


-- Ejercicio 24: Promedio
promedio : List Float -> Float
promedio list =
    case list of
        [] ->
            0.0

        _ ->
            let
                suma =
                    List.sum list

                cantidad =
                    toFloat (List.length list)
            in
            suma / cantidad


-- Ejercicio 25: Palabras a Longitudes
longitudesPalabras : String -> List Int
longitudesPalabras oracion =
    oracion
        |> String.words
        |> List.map String.length


-- Ejercicio 26: Remover Palabras Cortas
palabrasLargas : String -> List String
palabrasLargas oracion =
    oracion
        |> String.words
        |> List.filter (\palabra -> String.length palabra > 3)


-- Ejercicio 27: Sumar Números Positivos
sumarPositivos : List Int -> Int
sumarPositivos list =
    list
        |> List.filter (\x -> x > 0)
        |> List.sum


-- Ejercicio 28: Duplicar Pares
duplicarPares : List Int -> List Int
duplicarPares list =
    List.map
        (\x ->
            if modBy 2 x == 0 then
                x * 2

            else
                x
        )
        list



{- ========== PARTE 5: Desafíos Avanzados ========== -}


-- Ejercicio 29: Aplanar
aplanar : List (List a) -> List a
aplanar listaDeListas =
    List.foldl (++) [] listaDeListas


-- Ejercicio 30: Agrupar Por
agruparPor : (a -> a -> Bool) -> List a -> List (List a)
agruparPor equals list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( iguales, diferentes ) =
                    List.foldl
                        (\elem ( ig, dif ) ->
                            if equals x elem then
                                ( ig ++ [ elem ], dif )

                            else
                                ( ig, dif ++ [ elem ] )
                        )
                        ( [ x ], [] )
                        xs
            in
            iguales :: agruparPor equals diferentes


-- Ejercicio 31: Particionar
particionar : (a -> Bool) -> List a -> ( List a, List a )
particionar pred list =
    List.foldl
        (\elem ( verdaderos, falsos ) ->
            if pred elem then
                ( verdaderos ++ [ elem ], falsos )

            else
                ( verdaderos, falsos ++ [ elem ] )
        )
        ( [], [] )
        list


-- Ejercicio 32: Suma Acumulada
sumaAcumulada : List Int -> List Int
sumaAcumulada list =
    let
        ( _, resultado ) =
            List.foldl
                (\elem ( acum, resultados ) ->
                    let
                        nuevoAcum =
                            acum + elem
                    in
                    ( nuevoAcum, resultados ++ [ nuevoAcum ] )
                )
                ( 0, [] )
                list
    in
    resultado



{- ========== Ejercicios Adicionales: Subconjuntos y Dividir ========== -}


-- Subconjuntos
subSets : List Int -> List (List Int)
subSets list =
    List.foldl
        (\x acc -> acc ++ List.map (\s -> s ++ [ x ]) acc)
        [ [] ]
        list


-- Dividir en Grupos
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
                    cabeza =
                        tomar n list

                    resto =
                        saltar n list
                in
                cabeza :: cortar resto n



{- ========== BONUS: Diferencia entre foldl y foldr ========== -}


{-
   Pregunta: ¿Cuál es la diferencia entre foldl y foldr?

   List.foldl (::) [] [1, 2, 3]  -> [3, 2, 1]
   List.foldr (::) [] [1, 2, 3]  -> [1, 2, 3]

   Explicación:
   - foldl procesa la lista de izquierda a derecha, acumulando desde el inicio
   - foldr procesa la lista de derecha a izquierda, acumulando desde el final
   - Con (::), foldl invierte la lista porque agrega elementos al principio del acumulador
   - foldr mantiene el orden original porque construye la lista desde el final
-}