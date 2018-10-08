module Bingo.Utils exposing
    ( bounded
    , resultTask
    , split
    , swap
    )

import Html exposing (Html)
import Html.Attributes as Html
import Html5.DragDrop as DragDrop
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
