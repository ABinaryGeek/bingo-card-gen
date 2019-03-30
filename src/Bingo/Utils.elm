module Bingo.Utils exposing
    ( bounded
    , flattenResult
    , httpErrorToString
    , partition
    , singletonOrEmpty
    , split
    , swap
    )

import Http


bounded : Int -> Int -> Int -> Int
bounded minimum maximum value =
    min maximum (max minimum value)


split : Int -> List a -> ( List a, List a )
split amount list =
    let
        before =
            List.take amount list

        after =
            List.drop amount list
    in
    ( before, after )


swap : a -> a -> List a -> List a
swap this that values =
    List.map
        (\value ->
            if value == this then
                that

            else if value == that then
                this

            else
                value
        )
        values


singletonOrEmpty : Maybe a -> List a
singletonOrEmpty maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


partition : String -> String -> ( Maybe String, String )
partition separator string =
    case String.indexes separator string of
        [] ->
            ( Nothing, string )

        firstIndex :: _ ->
            ( Just (String.left firstIndex string)
            , String.dropLeft (firstIndex + String.length separator) string
            )


flattenResult : Result x (Result x a) -> Result x a
flattenResult result =
    case result of
        Ok innerResult ->
            innerResult

        Err error ->
            Err error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was not valid."

        Http.Timeout ->
            "The request timed out."

        Http.NetworkError ->
            "Your internet connection was interrupted."

        Http.BadStatus status ->
            "The server gave an unexpected status: " ++ String.fromInt status

        Http.BadBody message ->
            "The server gave an unexpected result: "
                ++ message
