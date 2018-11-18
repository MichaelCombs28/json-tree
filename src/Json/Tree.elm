module Json.Tree exposing
    ( Tree(..)
    , decode
    , encode
    , foldl
    , foldr
    , get
    , getArray
    , getBoolean
    , getNumber
    , getObject
    , getString
    , height
    , isArray
    , isNull
    , isObject
    , put
    , traverse
    )

import Json.Decode as Decode
import Json.Encode as Encode


type Tree
    = String String
    | Number Float
    | Boolean Bool
    | Array (List Tree)
    | Object (List ( String, Tree ))
    | NullValue


get : List String -> Tree -> Result String Tree
get queue tree =
    case queue of
        [] ->
            Ok tree

        pointer :: queue_ ->
            case tree of
                Object fields ->
                    getWithKey_ pointer fields
                        |> Maybe.map Ok
                        |> Maybe.withDefault
                            ("Field "
                                ++ pointer
                                ++ " does not exist."
                                |> Err
                            )
                        |> Result.andThen (get queue_)

                Array elements ->
                    case String.toInt pointer of
                        Just index ->
                            List.drop index elements
                                |> List.head
                                |> Maybe.map Ok
                                |> Maybe.withDefault
                                    ("Array element "
                                        ++ pointer
                                        ++ " does not exist."
                                        |> Err
                                    )
                                |> Result.andThen (get queue_)

                        Nothing ->
                            Err "Pointer was not a valid index."

                _ ->
                    case String.toInt pointer of
                        Just _ ->
                            Err "Tree node was not an array."

                        Nothing ->
                            Err "Tree node was not an object."


put : List String -> Tree -> Tree -> Result String Tree
put queue data structure =
    case queue of
        [] ->
            Ok data

        pointer :: queue_ ->
            case structure of
                Array elements ->
                    case String.toInt pointer of
                        Just index ->
                            insertArray_ queue_ data 0 index elements
                                |> Result.map Array

                        Nothing ->
                            Err "Pointer is not a valid array index."

                Object fields ->
                    insertObject_ queue_ data pointer fields
                        |> Result.map Object

                _ ->
                    case String.toInt pointer of
                        Just _ ->
                            Err "Invalid pointer, field is not an array or object."

                        Nothing ->
                            Err "Invalid pointer, field is not an object."


decode : Decode.Decoder Tree
decode =
    let
        lazy =
            Decode.lazy (\_ -> decode)
    in
    Decode.oneOf
        [ Decode.keyValuePairs lazy
            |> Decode.map Object
        , Decode.list lazy
            |> Decode.map Array
        , Decode.string
            |> Decode.map String
        , Decode.float
            |> Decode.map Number
        , Decode.bool
            |> Decode.map Boolean
        , Decode.null NullValue
        ]


encode : Tree -> Encode.Value
encode tree =
    case tree of
        String string ->
            Encode.string string

        Number float ->
            Encode.float float

        Boolean bool ->
            Encode.bool bool

        Array elements ->
            Encode.list encode elements

        Object elements ->
            List.map (Tuple.mapSecond encode) elements
                |> Encode.object

        NullValue ->
            Encode.null



-- Convenience


foldl : (List String -> Tree -> a -> a) -> a -> Tree -> a
foldl =
    foldl_ []


foldl_ :
    List String
    -> (List String -> Tree -> a -> a)
    -> a
    -> Tree
    -> a
foldl_ queue func accumulator tree =
    case tree of
        Object fields ->
            List.foldl
                (\( key, value ) acc ->
                    foldl_ (queue ++ [ key ]) func acc value
                )
                accumulator
                fields
                |> func queue tree

        Array elements ->
            List.foldl
                (\value ( index, acc ) ->
                    ( index + 1
                    , foldl_
                        (queue ++ [ String.fromInt index ])
                        func
                        acc
                        value
                    )
                )
                ( 0, accumulator )
                elements
                |> Tuple.second
                |> func queue tree

        others ->
            func queue others accumulator


foldr : (List String -> Tree -> a -> a) -> a -> Tree -> a
foldr =
    foldr_ []


foldr_ :
    List String
    -> (List String -> Tree -> a -> a)
    -> a
    -> Tree
    -> a
foldr_ queue func accumulator tree =
    case tree of
        Object fields ->
            List.foldr
                (\( key, value ) acc ->
                    foldr_ (queue ++ [ key ]) func acc value
                )
                accumulator
                fields
                |> func queue tree

        Array elements ->
            List.foldr
                (\value ( index, acc ) ->
                    ( index + 1
                    , foldr_
                        (queue ++ [ String.fromInt index ])
                        func
                        acc
                        value
                    )
                )
                ( 0, accumulator )
                elements
                |> Tuple.second
                |> func queue tree

        others ->
            func queue others accumulator


traverse : Tree -> List (List String)
traverse =
    traverse_ []


traverse_ : List String -> Tree -> List (List String)
traverse_ queue tree =
    case tree of
        Object fields ->
            List.map
                (\( key, value ) ->
                    traverse_ (queue ++ [ key ]) value
                )
                fields
                |> List.concat
                |> (++) [ queue ]

        Array elements ->
            List.indexedMap
                (\key value ->
                    traverse_ (queue ++ [ String.fromInt key ]) value
                )
                elements
                |> List.concat
                |> (++) [ queue ]

        _ ->
            [ queue ]


height : Tree -> Int
height tree =
    height_ 0 tree


height_ : Int -> Tree -> Int
height_ current tree =
    case tree of
        Object fields ->
            List.map (Tuple.second >> height_ current) fields
                |> List.maximum
                |> Maybe.withDefault 1
                |> (+) 1

        Array elements ->
            List.map (height_ current) elements
                |> List.maximum
                |> Maybe.withDefault 1
                |> (+) 1

        _ ->
            current + 1


isNull : Tree -> Bool
isNull tree =
    case tree of
        NullValue ->
            True

        _ ->
            False


isObject : Tree -> Bool
isObject tree =
    case tree of
        Object _ ->
            True

        _ ->
            False


isArray : Tree -> Bool
isArray tree =
    case tree of
        Array _ ->
            True

        _ ->
            False


getString : List String -> Tree -> Result String String
getString queue =
    get queue
        >> Result.andThen
            (\tree ->
                case tree of
                    String string ->
                        Ok string

                    _ ->
                        renderError_ "string" tree
            )


getNumber : List String -> Tree -> Result String Float
getNumber queue =
    get queue
        >> Result.andThen
            (\tree ->
                case tree of
                    Number num ->
                        Ok num

                    _ ->
                        renderError_ "number" tree
            )


getBoolean : List String -> Tree -> Result String Bool
getBoolean queue =
    get queue
        >> Result.andThen
            (\tree ->
                case tree of
                    Boolean bool ->
                        Ok bool

                    _ ->
                        renderError_ "boolean" tree
            )


getArray : List String -> Tree -> Result String (List Tree)
getArray queue =
    get queue
        >> Result.andThen
            (\tree ->
                case tree of
                    Array array ->
                        Ok array

                    _ ->
                        renderError_ "array" tree
            )


getObject : List String -> Tree -> Result String (List ( String, Tree ))
getObject queue =
    get queue
        >> Result.andThen
            (\tree ->
                case tree of
                    Object fields ->
                        Ok fields

                    _ ->
                        renderError_ "object" tree
            )



-- HELPERS


insertArray_ : List String -> Tree -> Int -> Int -> List Tree -> Result String (List Tree)
insertArray_ queue data current index elements =
    case elements of
        [] ->
            Err "Index out of range."

        head :: tail ->
            if index == current then
                Result.map2 (::)
                    (put queue data head)
                    (Ok tail)

            else
                insertArray_ queue data (current + 1) index tail
                    |> Result.map2 (::) (Ok head)


insertObject_ :
    List String
    -> Tree
    -> String
    -> List ( String, Tree )
    -> Result String (List ( String, Tree ))
insertObject_ queue data key values =
    case values of
        [] ->
            Err "Field did not exist."

        ( key_, value ) :: tail ->
            if key == key_ then
                let
                    next =
                        put queue data value
                            |> Result.map (\a -> ( key_, a ))
                in
                Result.map2 (::) next (Ok tail)

            else
                Result.map2 (::)
                    (Ok ( key_, value ))
                    (insertObject_ queue data key tail)


getWithKey_ : String -> List ( String, a ) -> Maybe a
getWithKey_ key values =
    case values of
        [] ->
            Nothing

        ( key_, value ) :: tail ->
            if key_ == key then
                Just value

            else
                getWithKey_ key tail


renderError_ : String -> Tree -> Result String a
renderError_ expectation tree =
    "Node was not of type "
        ++ expectation
        ++ " but of type: "
        ++ toTypeString_ tree
        |> Err


toTypeString_ : Tree -> String
toTypeString_ tree =
    case tree of
        String _ ->
            "string"

        Number _ ->
            "number"

        Boolean _ ->
            "boolean"

        Array _ ->
            "array"

        Object _ ->
            "object"

        NullValue ->
            "null"
