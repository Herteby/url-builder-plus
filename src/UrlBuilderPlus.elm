module UrlBuilderPlus exposing
    ( absolute, relative, crossOrigin, custom, Root(..)
    , QueryParameter, string, nonEmptyString, int, float, bool, maybe, list, bracketedList, toQuery
    )

{-| This module is identical to [Url.Builder](/packages/elm/url/latest/Url-Builder) found in [elm/url](/packages/elm/url/latest/), except that it has been extended with the following functions:

  - [`nonEmptyString`](#nonEmptyString)
  - [`float`](#float)
  - [`bool`](#bool)
  - [`maybe`](#maybe)
  - [`list`](#list)
  - [`bracketedList`](#bracketedList)


# Builders

@docs absolute, relative, crossOrigin, custom, Root


# Queries

@docs QueryParameter, string, nonEmptyString, int, float, bool, maybe, list, bracketedList, toQuery

-}

import Url



-- BUILDERS


{-| Create an absolute URL:

    absolute [] []
    -- "/"

    absolute [ "packages", "elm", "core" ] []
    -- "/packages/elm/core"

    absolute [ "blog", String.fromInt 42 ] []
    -- "/blog/42"

    absolute [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Notice that the URLs start with a slash!

-}
absolute : List String -> List QueryParameter -> String
absolute pathSegments parameters =
    "/" ++ String.join "/" pathSegments ++ toQuery parameters


{-| Create a relative URL:

    relative [] []
    -- ""

    relative [ "elm", "core" ] []
    -- "elm/core"

    relative [ "blog", String.fromInt 42 ] []
    -- "blog/42"

    relative [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "products?search=hat&page=2"

Notice that the URLs **do not** start with a slash!

-}
relative : List String -> List QueryParameter -> String
relative pathSegments parameters =
    String.join "/" pathSegments ++ toQuery parameters


{-| Create a cross-origin URL.

    crossOrigin "https://example.com" [ "products" ] []
    -- "https://example.com/products"

    crossOrigin "https://example.com" [] []
    -- "https://example.com/"

    crossOrigin
      "https://example.com:8042"
      [ "over", "there" ]
      [ string "name" "ferret" ]
    -- "https://example.com:8042/over/there?name=ferret"

**Note:** Cross-origin requests are slightly restricted for security.
For example, the [same-origin policy][sop] applies when sending HTTP requests,
so the appropriate `Access-Control-Allow-Origin` header must be enabled on the
_server_ to get things working. Read more about the security rules [here][cors].

[sop]: https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy
[cors]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

-}
crossOrigin : String -> List String -> List QueryParameter -> String
crossOrigin prePath pathSegments parameters =
    prePath ++ "/" ++ String.join "/" pathSegments ++ toQuery parameters



-- CUSTOM BUILDER


{-| Specify whether a [`custom`](#custom) URL is absolute, relative, or
cross-origin.
-}
type Root
    = Absolute
    | Relative
    | CrossOrigin String


{-| Create custom URLs that may have a hash on the end:

    custom Absolute
      [ "packages", "elm", "core", "latest", "String" ]
      []
      (Just "length")
    -- "/packages/elm/core/latest/String#length"

    custom Relative [ "there" ] [ string "name" "ferret" ] Nothing
    -- "there?name=ferret"

    custom
      (CrossOrigin "https://example.com:8042")
      [ "over", "there" ]
      [ string "name" "ferret" ]
      (Just "nose")
    -- "https://example.com:8042/over/there?name=ferret#nose"

-}
custom : Root -> List String -> List QueryParameter -> Maybe String -> String
custom root pathSegments parameters maybeFragment =
    let
        fragmentless =
            rootToPrePath root ++ String.join "/" pathSegments ++ toQuery parameters
    in
    case maybeFragment of
        Nothing ->
            fragmentless

        Just fragment ->
            fragmentless ++ "#" ++ fragment


rootToPrePath : Root -> String
rootToPrePath root =
    case root of
        Absolute ->
            "/"

        Relative ->
            ""

        CrossOrigin prePath ->
            prePath ++ "/"



-- QUERY PARAMETERS


{-| Represents query parameter. Builder functions like `absolute` percent-encode
all the query parameters they get, so you do not need to worry about it!
-}
type QueryParameter
    = QueryParameter String String
    | None


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat" ]
    -- "/products?search=hat"

    absolute ["products"] [ string "search" "coffee table" ]
    -- "/products?search=coffee%20table"

-}
string : String -> String -> QueryParameter
string key value =
    QueryParameter (Url.percentEncode key) (Url.percentEncode value)


{-| Create a percent-encoded query parameter. If the string is empty, the parameter is omitted

    absolute ["products"] [ string "search" "hat" ]
    -- "/products?search=hat"

    absolute ["products"] [ string "search" "" ]
    -- "/products"

-}
nonEmptyString : String -> String -> QueryParameter
nonEmptyString key value =
    if value == "" then
        None

    else
        QueryParameter (Url.percentEncode key) (Url.percentEncode value)


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Writing `int key n` is the same as writing `string key (String.fromInt n)`.
So this is just a convenience function, making your code a bit shorter!

-}
int : String -> Int -> QueryParameter
int key value =
    QueryParameter (Url.percentEncode key) (String.fromInt value)


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", float "maxprice" 9.99 ]
    -- "/products?search=hat&maxprice=9.99"

-}
float : String -> Float -> QueryParameter
float key value =
    QueryParameter (Url.percentEncode key) (String.fromFloat value)


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", bool "discounted" True ]
    -- "/products?search=hat&discounted=true"

-}
bool : String -> Bool -> QueryParameter
bool key value =
    QueryParameter (Url.percentEncode key)
        (if value then
            "true"

         else
            "false"
        )


{-| Create a percent-encoded query parameter. If the value is Nothing, the parameter is omitted

    absolute ["products"] [ string "search" "hat", maybe float "maxprice" (Just 9.99) ]
    -- "/products?search=hat&maxprice=9.99"

    absolute ["products"] [ string "search" "hat", maybe float "maxprice" Nothing ]
    -- "/products?search=hat"

-}
maybe : (String -> a -> QueryParameter) -> String -> Maybe a -> QueryParameter
maybe encoder key value =
    case value of
        Just a ->
            encoder key a

        Nothing ->
            None


{-| Make a comma-separated list. If the list is empty, the parameter is omitted

    absolute ["products"] [ string "search" "hat", list int "sizes" [1,2,3] ]
    -- "/products?search=hat&sizes=1,2,3"
    -- actually it becomes "/products?search=hat&sizes=1%2C2%2C3" since it's url-encoded, but don't worry about it

    absolute ["products"] [ string "search" "hat", list int "sizes" [] ]
    -- "/products?search=hat"

-}
list : (String -> a -> QueryParameter) -> String -> List a -> QueryParameter
list encoder key values =
    if values == [] then
        None

    else
        values
            |> List.map (encoder "")
            |> List.filterMap
                (\p ->
                    case p of
                        QueryParameter _ str ->
                            Just str

                        None ->
                            Nothing
                )
            |> String.join ","
            |> string key


{-| Make a comma-separated list surrounded by brackets (in case your API uses that list syntax).
If the list is empty, the parameter is omitted

    absolute ["products"] [ string "search" "hat", list int "sizes" [1,2,3] ]
    -- "/products?search=hat&sizes=[1,2,3]"
    -- actually it becomes "/products?search=hat&sizes=[1%2C2%2C3]" since it's url-encoded, but don't worry about it

    absolute ["products"] [ string "search" "hat", list int "sizes" [] ]
    -- "/products?search=hat"

-}
bracketedList : (String -> a -> QueryParameter) -> String -> List a -> QueryParameter
bracketedList encoder key values =
    list encoder key values
        |> (\p ->
                case p of
                    QueryParameter k s ->
                        QueryParameter k ("[" ++ s ++ "]")

                    None ->
                        None
           )


{-| Convert a list of query parameters to a percent-encoded query. This
function is used by `absolute`, `relative`, etc.

    toQuery [ string "search" "hat" ]
    -- "?search=hat"

    toQuery [ string "search" "coffee table" ]
    -- "?search=coffee%20table"

    toQuery [ string "search" "hat", int "page" 2 ]
    -- "?search=hat&page=2"

    toQuery []
    -- ""

-}
toQuery : List QueryParameter -> String
toQuery parameters =
    case parameters |> List.filter (\p -> p /= None) of
        [] ->
            ""

        params ->
            "?" ++ String.join "&" (List.map toQueryPair params)


toQueryPair : QueryParameter -> String
toQueryPair param =
    case param of
        QueryParameter key value ->
            key ++ "=" ++ value

        None ->
            ""
