module Tests.JsonTree exposing (suite, testJSON)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode
import Json.Encode
import Json.Tree exposing (Tree(..))
import Test exposing (..)


suite : Test
suite =
    describe "Test cases for json-tree"
        [ test "Test decode" <|
            \_ ->
                Json.Decode.decodeString Json.Tree.decode testJSON
                    |> Result.mapError Json.Decode.errorToString
                    |> fromResult jsonTree
        , test "Test encode" <|
            \_ ->
                Json.Tree.encode (Json.Tree.Array [ jsonElement ])
                    |> Json.Encode.encode 4
                    |> Expect.equal testJSON
        , test "Test get normal field" <|
            \_ ->
                Json.Tree.get [ "id" ] jsonElement
                    |> fromResult (Number 1)
        , test "Test get nested field" <|
            \_ ->
                Json.Tree.get [ "address", "city" ] jsonElement
                    |> fromResult (String "San Jose")
        , test "Test get within array" <|
            \_ ->
                Json.Tree.get [ "0", "interests", "1" ] jsonTree
                    |> fromResult (String "programming")
        , test "Test put within object." <|
            \_ ->
                Json.Tree.put [ "0", "interests", "0" ] (String "cars") jsonTree
                    |> Result.andThen (Json.Tree.get [ "0", "interests", "0" ])
                    |> fromResult (String "cars")
        , test "Test height" <|
            \_ ->
                Json.Tree.height jsonTree
                    |> Expect.equal 4
        , test "Test traverse" <|
            \_ ->
                Json.Tree.traverse jsonTree
                    |> Expect.equal
                        [ []
                        , [ "0" ]
                        , [ "0", "id" ]
                        , [ "0", "interests" ]
                        , [ "0", "interests", "0" ]
                        , [ "0", "interests", "1" ]
                        , [ "0", "alive" ]
                        , [ "0", "name" ]
                        , [ "0", "address" ]
                        , [ "0", "address", "city" ]
                        , [ "0", "address", "street" ]
                        ]
        , test "Test fold" <|
            \_ ->
                let
                    transform queue node acc =
                        if
                            Json.Tree.isObject node
                                || Json.Tree.isArray node
                                || Json.Tree.isNull node
                        then
                            acc

                        else
                            Dict.insert queue node acc

                    expectation =
                        [ ( [ "id" ], Number 1 )
                        , ( [ "interests", "0" ], String "school" )
                        , ( [ "interests", "1" ], String "programming" )
                        , ( [ "alive" ], Boolean True )
                        , ( [ "name" ], String "Alyss Hambrick" )
                        , ( [ "address", "city" ], String "San Jose" )
                        , ( [ "address", "street" ], String "900 South 1st" )
                        ]
                            |> Dict.fromList
                in
                Json.Tree.foldl transform Dict.empty jsonElement
                    |> Expect.equal expectation
        ]


fromResult : a -> Result String a -> Expectation
fromResult expectation result =
    case result of
        Ok a ->
            Expect.equal expectation a

        Err e ->
            Expect.fail e


testJSON : String
testJSON =
    """[
    {
        "id": 1,
        "interests": [
            "school",
            "programming"
        ],
        "alive": true,
        "name": "Alyss Hambrick",
        "address": {
            "city": "San Jose",
            "street": "900 South 1st"
        }
    }
]"""


jsonElement : Tree
jsonElement =
    Object
        [ ( "id", Number 1 )
        , ( "interests", Array [ String "school", String "programming" ] )
        , ( "alive", Boolean True )
        , ( "name", String "Alyss Hambrick" )
        , ( "address"
          , Object
                [ ( "city", String "San Jose" )
                , ( "street", String "900 South 1st" )
                ]
          )
        ]


jsonTree : Tree
jsonTree =
    Array
        [ jsonElement
        ]
