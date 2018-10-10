module Bingo.Utils exposing
    ( bounded
    , flattenResult
    , nonEmptyList
    , partition
    , resultTask
    , singletonOrEmpty
    , split
    , swap
    )

import Html exposing (Html)
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
