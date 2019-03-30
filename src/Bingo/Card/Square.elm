module Bingo.Card.Square exposing (map, squares, text)

import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Model exposing (..)
import Bingo.Utils as Utils


squares : Layout -> List Value -> List Square
squares layout values =
    let
        amount =
            Layout.amountOfValues layout

        ( used, _ ) =
            Utils.split amount values

        actualAmount =
            List.length used

        extraNeeded =
            amount - actualAmount

        padded =
            List.map Filled used ++ List.repeat extraNeeded Unfilled
    in
    case Layout.freeSquareUsed layout of
        True ->
            let
                half =
                    amount // 2
            in
            List.concat
                [ List.take half padded
                , [ Free ]
                , List.drop half padded
                ]

        False ->
            padded


map : (Value -> List x) -> Square -> List x
map f sq =
    case sq of
        Filled value ->
            f value

        _ ->
            []


text : Square -> String
text sq =
    case sq of
        Filled value ->
            value

        Unfilled ->
            ""

        Free ->
            "FREE"
