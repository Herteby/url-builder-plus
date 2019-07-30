module Tests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import UrlBuilderPlus as UB


suite : Test
suite =
    describe "UrlBuilderPlus"
        [ describe "nonEmptyString"
            [ test "not empty" <|
                \_ -> UB.absolute [] [ UB.nonEmptyString "foo" "foo" ] |> Expect.equal "/?foo=foo"
            , test "empty" <|
                \_ -> UB.absolute [] [ UB.nonEmptyString "foo" "" ] |> Expect.equal "/"
            ]
        , describe "float"
            [ test "works" <|
                \_ -> UB.absolute [] [ UB.float "foo" 4.2 ] |> Expect.equal "/?foo=4.2"
            ]
        , describe "bool"
            [ test "true" <|
                \_ -> UB.absolute [] [ UB.bool "foo" True ] |> Expect.equal "/?foo=true"
            , test "false" <|
                \_ -> UB.absolute [] [ UB.bool "foo" False ] |> Expect.equal "/?foo=false"
            ]
        , describe "maybe"
            [ test "just" <|
                \_ -> UB.absolute [] [ UB.maybe UB.string "foo" (Just "foo") ] |> Expect.equal "/?foo=foo"
            , test "nothing" <|
                \_ -> UB.absolute [] [ UB.maybe UB.string "foo" Nothing ] |> Expect.equal "/"
            ]
        , describe "list"
            [ test "not empty" <|
                \_ -> UB.absolute [] [ UB.list UB.int "foo" [ 1, 2, 3 ] ] |> Expect.equal "/?foo=1%2C2%2C3"
            , test "empty" <|
                \_ -> UB.absolute [] [ UB.list UB.int "foo" [] ] |> Expect.equal "/"
            ]
        , describe "bracketedList"
            [ test "not empty" <|
                \_ -> UB.absolute [] [ UB.bracketedList UB.int "foo" [ 1, 2, 3 ] ] |> Expect.equal "/?foo=[1%2C2%2C3]"
            , test "empty" <|
                \_ -> UB.absolute [] [ UB.bracketedList UB.int "foo" [] ] |> Expect.equal "/"
            ]
        ]
