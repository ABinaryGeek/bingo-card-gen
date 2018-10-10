module Bingo.Utils exposing
    ( bounded
    , flattenResult
    , httpErrorToString
    , nonEmptyList
    , partition
    , resultTask
    , singletonOrEmpty
    , split
    , swap
    )

import Html exposing (Html)
import Http
import Random
import Task exposing (Task)


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


resultTask : Result x a -> Task x a
resultTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x


singletonOrEmpty : Maybe a -> List a
singletonOrEmpty maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


nonEmptyList : List a -> Maybe ( a, List a )
nonEmptyList list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, rest )


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

        Http.BadStatus response ->
            "The server gave an unexpected " ++ httpResponseToString response

        Http.BadPayload message response ->
            "The server gave an unexpected result: "
                ++ message
                ++ " after giving the "
                ++ httpResponseToString response



{- Private -}


httpResponseToString : Http.Response String -> String
httpResponseToString response =
    "response: '" ++ response.status.message ++ "' Body: '" ++ response.body ++ "'"
