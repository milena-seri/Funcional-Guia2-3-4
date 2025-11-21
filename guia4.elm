-- filepath: c:\Users\elcha\paradigmaUAP2025\funcional\ejercicios\ejercicio4\guia4.elm
module Guia4 exposing (..)


{- Guía 4 - Pattern Matching y Mónadas con Árboles Binarios -}


{- ========== PARTE 0: Definición del Árbol Binario ========== -}


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)



{- Ejercicios de Construcción -}


-- Ejercicio 1: Crear Árboles de Ejemplo
arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- Ejercicio 2: Es Vacío
esVacio : Tree a -> Bool
esVacio tree =
    case tree of
        Empty ->
            True

        Node _ _ _ ->
            False


-- Ejercicio 3: Es Hoja
esHoja : Tree a -> Bool
esHoja tree =
    case tree of
        Empty ->
            False

        Node _ Empty Empty ->
            True

        Node _ _ _ ->
            False



{- ========== PARTE 1: Pattern Matching con Árboles ========== -}


-- Ejercicio 4: Tamaño del Árbol
tamaño : Tree a -> Int
tamaño tree =
    case tree of
        Empty ->
            0

        Node _ left right ->
            1 + tamaño left + tamaño right


-- Ejercicio 5: Altura del Árbol
altura : Tree a -> Int
altura tree =
    case tree of
        Empty ->
            0

        Node _ left right ->
            1 + max (altura left) (altura right)


-- Ejercicio 6: Suma de Valores
sumarArbol : Tree Int -> Int
sumarArbol tree =
    case tree of
        Empty ->
            0

        Node value left right ->
            value + sumarArbol left + sumarArbol right


-- Ejercicio 7: Contiene Valor
contiene : a -> Tree a -> Bool
contiene target tree =
    case tree of
        Empty ->
            False

        Node value left right ->
            if value == target then
                True

            else
                contiene target left || contiene target right


-- Ejercicio 8: Contar Hojas
contarHojas : Tree a -> Int
contarHojas tree =
    case tree of
        Empty ->
            0

        Node _ Empty Empty ->
            1

        Node _ left right ->
            contarHojas left + contarHojas right


-- Ejercicio 9: Valor Mínimo (sin Maybe)
minimo : Tree Int -> Int
minimo tree =
    case tree of
        Empty ->
            0

        Node value Empty Empty ->
            value

        Node value left right ->
            let
                minLeft =
                    minimo left

                minRight =
                    minimo right

                minSubtrees =
                    if minLeft == 0 && minRight == 0 then
                        value

                    else if minLeft == 0 then
                        min value minRight

                    else if minRight == 0 then
                        min value minLeft

                    else
                        min value (min minLeft minRight)
            in
            minSubtrees


-- Ejercicio 10: Valor Máximo (sin Maybe)
maximo : Tree Int -> Int
maximo tree =
    case tree of
        Empty ->
            0

        Node value Empty Empty ->
            value

        Node value left right ->
            let
                maxLeft =
                    maximo left

                maxRight =
                    maximo right
            in
            max value (max maxLeft maxRight)



{- ========== PARTE 2: Introducción a Maybe ========== -}


-- Ejercicio 11: Buscar Valor
buscar : a -> Tree a -> Maybe a
buscar target tree =
    case tree of
        Empty ->
            Nothing

        Node value left right ->
            if value == target then
                Just value

            else
                case buscar target left of
                    Just found ->
                        Just found

                    Nothing ->
                        buscar target right


-- Ejercicio 12: Encontrar Mínimo (con Maybe)
encontrarMinimo : Tree comparable -> Maybe comparable
encontrarMinimo tree =
    case tree of
        Empty ->
            Nothing

        Node value Empty Empty ->
            Just value

        Node value left right ->
            let
                minLeft =
                    encontrarMinimo left

                minRight =
                    encontrarMinimo right
            in
            case ( minLeft, minRight ) of
                ( Nothing, Nothing ) ->
                    Just value

                ( Just l, Nothing ) ->
                    Just (min value l)

                ( Nothing, Just r ) ->
                    Just (min value r)

                ( Just l, Just r ) ->
                    Just (min value (min l r))


-- Ejercicio 13: Encontrar Máximo (con Maybe)
encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo tree =
    case tree of
        Empty ->
            Nothing

        Node value Empty Empty ->
            Just value

        Node value left right ->
            let
                maxLeft =
                    encontrarMaximo left

                maxRight =
                    encontrarMaximo right
            in
            case ( maxLeft, maxRight ) of
                ( Nothing, Nothing ) ->
                    Just value

                ( Just l, Nothing ) ->
                    Just (max value l)

                ( Nothing, Just r ) ->
                    Just (max value r)

                ( Just l, Just r ) ->
                    Just (max value (max l r))


-- Ejercicio 14: Buscar Por Predicado
buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor pred tree =
    case tree of
        Empty ->
            Nothing

        Node value left right ->
            if pred value then
                Just value

            else
                case buscarPor pred left of
                    Just found ->
                        Just found

                    Nothing ->
                        buscarPor pred right


-- Ejercicio 15: Obtener Valor de Raíz
raiz : Tree a -> Maybe a
raiz tree =
    case tree of
        Empty ->
            Nothing

        Node value _ _ ->
            Just value


-- Ejercicio 16: Obtener Hijo Izquierdo y Derecho
hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo tree =
    case tree of
        Empty ->
            Nothing

        Node _ left _ ->
            if esVacio left then
                Nothing

            else
                Just left


hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho tree =
    case tree of
        Empty ->
            Nothing

        Node _ _ right ->
            if esVacio right then
                Nothing

            else
                Just right


-- Ejercicio 17: Obtener Nieto
nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo tree =
    hijoIzquierdo tree
        |> Maybe.andThen hijoIzquierdo


-- Ejercicio 18: Buscar en Profundidad
obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol target tree =
    case tree of
        Empty ->
            Nothing

        Node value left right ->
            if value == target then
                Just tree

            else
                case obtenerSubarbol target left of
                    Just found ->
                        Just found

                    Nothing ->
                        obtenerSubarbol target right


buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 tree =
    obtenerSubarbol valor1 tree
        |> Maybe.andThen (\subtree -> buscar valor2 subtree)



{- ========== PARTE 3: Result para Validaciones ========== -}


-- Ejercicio 19: Validar No Vacío
validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio tree =
    case tree of
        Empty ->
            Err "El árbol está vacío"

        _ ->
            Ok tree


-- Ejercicio 20: Obtener Raíz con Error
obtenerRaiz : Tree a -> Result String a
obtenerRaiz tree =
    case tree of
        Empty ->
            Err "No se puede obtener la raíz de un árbol vacío"

        Node value _ _ ->
            Ok value


-- Ejercicio 21: Dividir en Valor Raíz y Subárboles
dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir tree =
    case tree of
        Empty ->
            Err "No se puede dividir un árbol vacío"

        Node value left right ->
            Ok ( value, left, right )


-- Ejercicio 22: Obtener Mínimo con Error
obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo tree =
    case encontrarMinimo tree of
        Nothing ->
            Err "No hay mínimo en un árbol vacío"

        Just value ->
            Ok value


-- Ejercicio 23: Verificar si es BST
esBSTHelper : Maybe comparable -> Maybe comparable -> Tree comparable -> Bool
esBSTHelper minVal maxVal tree =
    case tree of
        Empty ->
            True

        Node value left right ->
            let
                validMin =
                    case minVal of
                        Nothing ->
                            True

                        Just min ->
                            value > min

                validMax =
                    case maxVal of
                        Nothing ->
                            True

                        Just max ->
                            value < max
            in
            validMin
                && validMax
                && esBSTHelper minVal (Just value) left
                && esBSTHelper (Just value) maxVal right


esBST : Tree comparable -> Bool
esBST tree =
    esBSTHelper Nothing Nothing tree


-- Ejercicio 24: Insertar en BST
insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST value tree =
    case tree of
        Empty ->
            Ok (Node value Empty Empty)

        Node nodeValue left right ->
            if value == nodeValue then
                Err ("El valor " ++ Debug.toString value ++ " ya existe en el árbol")

            else if value < nodeValue then
                insertarBST value left
                    |> Result.map (\newLeft -> Node nodeValue newLeft right)

            else
                insertarBST value right
                    |> Result.map (\newRight -> Node nodeValue left newRight)


-- Ejercicio 25: Buscar en BST
buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST target tree =
    case tree of
        Empty ->
            Err ("El valor " ++ Debug.toString target ++ " no se encuentra en el árbol")

        Node value left right ->
            if target == value then
                Ok value

            else if target < value then
                buscarEnBST target left

            else
                buscarEnBST target right


-- Ejercicio 26: Validar BST con Result
validarBST : Tree comparable -> Result String (Tree comparable)
validarBST tree =
    if esBST tree then
        Ok tree

    else
        Err "El árbol no es un BST válido"



{- ========== PARTE 4: Combinando Maybe y Result ========== -}


-- Ejercicio 27: Maybe a Result
maybeAResult : String -> Maybe a -> Result String a
maybeAResult errorMsg maybe =
    case maybe of
        Nothing ->
            Err errorMsg

        Just value ->
            Ok value


-- Ejercicio 28: Result a Maybe
resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Err _ ->
            Nothing

        Ok value ->
            Just value


-- Ejercicio 29: Buscar y Validar
buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo target tree =
    case buscar target tree of
        Nothing ->
            Err ("El valor " ++ String.fromInt target ++ " no se encuentra en el árbol")

        Just value ->
            if value > 0 then
                Ok value

            else
                Err ("El valor " ++ String.fromInt value ++ " no es positivo")


-- Ejercicio 30: Pipeline de Validaciones
todosPositivos : Tree Int -> Bool
todosPositivos tree =
    case tree of
        Empty ->
            True

        Node value left right ->
            value > 0 && todosPositivos left && todosPositivos right


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol tree =
    validarNoVacio tree
        |> Result.andThen validarBST
        |> Result.andThen
            (\t ->
                if todosPositivos t then
                    Ok t

                else
                    Err "El árbol contiene valores no positivos"
            )


-- Ejercicio 31: Encadenar Búsquedas
buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    buscarEnBST valor arbol1
        |> Result.andThen (\resultadoIntermedio -> buscarEnBST resultadoIntermedio arbol2)



{- ========== PARTE 5: Desafíos Avanzados - Recorridos ========== -}


-- Ejercicio 32: Recorrido Inorder
inorder : Tree a -> List a
inorder tree =
    case tree of
        Empty ->
            []

        Node value left right ->
            inorder left ++ [ value ] ++ inorder right


-- Ejercicio 33: Recorrido Preorder
preorder : Tree a -> List a
preorder tree =
    case tree of
        Empty ->
            []

        Node value left right ->
            [ value ] ++ preorder left ++ preorder right


-- Ejercicio 34: Recorrido Postorder
postorder : Tree a -> List a
postorder tree =
    case tree of
        Empty ->
            []

        Node value left right ->
            postorder left ++ postorder right ++ [ value ]



{- ========== Transformaciones ========== -}


-- Ejercicio 35: Map sobre Árbol
mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol f tree =
    case tree of
        Empty ->
            Empty

        Node value left right ->
            Node (f value) (mapArbol f left) (mapArbol f right)


-- Ejercicio 36: Filter sobre Árbol
filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol pred tree =
    case tree of
        Empty ->
            Empty

        Node value left right ->
            let
                filteredLeft =
                    filterArbol pred left

                filteredRight =
                    filterArbol pred right
            in
            if pred value then
                Node value filteredLeft filteredRight

            else
                case ( filteredLeft, filteredRight ) of
                    ( Empty, Empty ) ->
                        Empty

                    ( Empty, _ ) ->
                        filteredRight

                    ( _, Empty ) ->
                        filteredLeft

                    ( _, _ ) ->
                        Node value filteredLeft filteredRight


-- Ejercicio 37: Fold sobre Árbol
foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol f acc tree =
    List.foldl f acc (inorder tree)



{- ========== BST Avanzado ========== -}


-- Ejercicio 38: Eliminar de BST
eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST target tree =
    case tree of
        Empty ->
            Err ("El valor " ++ Debug.toString target ++ " no existe en el árbol")

        Node value left right ->
            if target < value then
                eliminarBST target left
                    |> Result.map (\newLeft -> Node value newLeft right)

            else if target > value then
                eliminarBST target right
                    |> Result.map (\newRight -> Node value left newRight)

            else
                -- Nodo encontrado
                case ( left, right ) of
                    ( Empty, Empty ) ->
                        Ok Empty

                    ( Empty, _ ) ->
                        Ok right

                    ( _, Empty ) ->
                        Ok left

                    ( _, _ ) ->
                        -- Dos hijos: reemplazar con mínimo del subárbol derecho
                        case encontrarMinimo right of
                            Nothing ->
                                Err "Error al encontrar el sucesor"

                            Just minRight ->
                                eliminarBST minRight right
                                    |> Result.map (\newRight -> Node minRight left newRight)


-- Ejercicio 39: Construir BST desde Lista
desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST list =
    List.foldl
        (\valor resultArbol ->
            resultArbol
                |> Result.andThen (\arbol -> insertarBST valor arbol)
        )
        (Ok Empty)
        list


-- Ejercicio 40: Verificar Balance
estaBalanceado : Tree a -> Bool
estaBalanceado tree =
    let
        checkBalance t =
            case t of
                Empty ->
                    ( True, 0 )

                Node _ left right ->
                    let
                        ( leftBalanced, leftHeight ) =
                            checkBalance left

                        ( rightBalanced, rightHeight ) =
                            checkBalance right

                        heightDiff =
                            abs (leftHeight - rightHeight)

                        currentHeight =
                            1 + max leftHeight rightHeight
                    in
                    ( leftBalanced && rightBalanced && heightDiff <= 1, currentHeight )
    in
    Tuple.first (checkBalance tree)


-- Ejercicio 41: Balancear BST
balancear : Tree comparable -> Tree comparable
balancear tree =
    let
        sortedList =
            inorder tree

        buildBalanced list =
            let
                len =
                    List.length list
            in
            if len == 0 then
                Empty

            else
                let
                    mid =
                        len // 2

                    left =
                        List.take mid list

                    middle =
                        List.drop mid list |> List.head |> Maybe.withDefault (Debug.todo "Error en balancear")

                    right =
                        List.drop (mid + 1) list
                in
                Node middle (buildBalanced left) (buildBalanced right)
    in
    buildBalanced sortedList



{- ========== Path Finding ========== -}


type Direccion
    = Izquierda
    | Derecha


-- Ejercicio 42: Camino a un Valor
encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino target tree =
    let
        buscarCamino t path =
            case t of
                Empty ->
                    Nothing

                Node value left right ->
                    if value == target then
                        Just (List.reverse path)

                    else
                        case buscarCamino left (Izquierda :: path) of
                            Just found ->
                                Just found

                            Nothing ->
                                buscarCamino right (Derecha :: path)
    in
    case buscarCamino tree [] of
        Nothing ->
            Err ("El valor " ++ Debug.toString target ++ " no existe en el árbol")

        Just path ->
            Ok path


-- Ejercicio 43: Seguir Camino
seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino tree =
    case ( camino, tree ) of
        ( [], Empty ) ->
            Err "Árbol vacío"

        ( [], Node value _ _ ) ->
            Ok value

        ( Izquierda :: rest, Node _ left _ ) ->
            if esVacio left then
                Err "Camino inválido: no hay hijo izquierdo"

            else
                seguirCamino rest left

        ( Derecha :: rest, Node _ _ right ) ->
            if esVacio right then
                Err "Camino inválido: no hay hijo derecho"

            else
                seguirCamino rest right

        ( _, Empty ) ->
            Err "Camino inválido: árbol vacío"


-- Ejercicio 44: Ancestro Común Más Cercano
ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun val1 val2 tree =
    case tree of
        Empty ->
            Err "Árbol vacío"

        Node value left right ->
            if val1 < value && val2 < value then
                ancestroComun val1 val2 left

            else if val1 > value && val2 > value then
                ancestroComun val1 val2 right

            else
                -- Uno está a la izquierda y otro a la derecha, o uno es el nodo actual
                Ok value



{- ========== PARTE 6: Sistema Completo de BST ========== -}


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido =
    esBST


estaBalanceadoValido : Tree comparable -> Bool
estaBalanceadoValido =
    estaBalanceado


contieneValor : comparable -> Tree comparable -> Bool
contieneValor =
    contiene



-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe =
    buscar


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe =
    encontrarMinimo


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe =
    encontrarMaximo



-- Operaciones que retornan Result
insertar : comparable -> Tree comparable -> Result String (Tree comparable)
insertar =
    insertarBST


eliminar : comparable -> Tree comparable -> Result String (Tree comparable)
eliminar =
    eliminarBST


validar : Tree comparable -> Result String (Tree comparable)
validar =
    validarBST


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion pos tree =
    let
        lista =
            inorder tree

        elemento =
            List.drop pos lista |> List.head
    in
    case elemento of
        Nothing ->
            Err ("Posición " ++ String.fromInt pos ++ " fuera de rango")

        Just value ->
            Ok value



-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map =
    mapArbol


filter : (a -> Bool) -> Tree a -> Tree a
filter =
    filterArbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold =
    foldArbol



-- Conversiones
aLista : Tree a -> List a
aLista =
    inorder


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada list =
    case desdeListaBST list of
        Ok tree ->
            balancear tree

        Err _ ->
            Empty