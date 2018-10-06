module Bingo.Card exposing
    ( add
    , changeName
    , couldAdd
    , init
    , remove
    , resize
    , setValues
    , swap
    , toggleFreeSquare
    , view
    )

import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Model as Model exposing (Value)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Html
import Html5.DragDrop as DragDrop


init : Card
init =
    { name = "Bingo Card"
    , values = []
    , layout =
        { size = 5
        , free = True
        }
    }


couldAdd : Value -> Card -> Bool
couldAdd value card =
    if String.isEmpty value then
        False

    else
        case List.member value card.values of
            True ->
                False

            False ->
                True


add : Value -> Card -> Card
add value card =
    let
        toAdd =
            if couldAdd value card then
                [ value ]

            else
                []
    in
    setValues (card.values ++ toAdd) card


remove : Value -> Card -> Card
remove value card =
    setValues (List.filter (\v -> v /= value) card.values) card


setValues : List Value -> Card -> Card
setValues values card =
    { card | values = values }


changeName : String -> Card -> Card
changeName name card =
    { card | name = name }


resize : Int -> Card -> Card
resize size card =
    { card | layout = Layout.resize size card.layout }


toggleFreeSquare : Card -> Card
toggleFreeSquare card =
    { card | layout = Layout.toggleFreeSquare card.layout }


swap : Value -> Value -> Card -> Card
swap this that card =
    { card | values = Utils.swap this that card.values }


view : (Value -> List (Html.Attribute msg)) -> Card -> Html msg
view squareAttributes card =
    Html.div [ Html.class "bingo-card" ]
        [ Html.h1 [] [ Html.text card.name ]
        , grid squareAttributes card.layout card.values
        , Html.span [ Html.class "footer" ]
            [ Html.a [ Html.href "http://connieswap.tumblr.com/" ]
                [ Html.text "connieswap.tumblr.com"
                ]
            ]
        ]


grid : (Value -> List (Html.Attribute msg)) -> Layout -> List Value -> Html msg
grid squareAttributes layout values =
    Html.ul
        ([ Html.class "bingo-squares" ] ++ Layout.gridStyles layout)
        (List.map (square squareAttributes) (squares layout values))


squares : Layout -> List Value -> List Square
squares layout values =
    let
        amount =
            Layout.amountOfSquares layout

        ( used, unused ) =
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


square : (Value -> List (Html.Attribute msg)) -> Square -> Html msg
square attributes sq =
    Html.li
        ([ Html.class "bingo-square" ] ++ squareMap attributes sq)
        [ Html.span [] [ Html.text (squareText sq) ]
        ]


squareMap : (Value -> List x) -> Square -> List x
squareMap f sq =
    case sq of
        Filled value ->
            f value

        _ ->
            []


squareText : Square -> String
squareText sq =
    case sq of
        Filled value ->
            value

        Unfilled ->
            ""

        Free ->
            "FREE"
