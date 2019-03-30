module Bingo.Card exposing
    ( add
    , addMany
    , changeName
    , couldAdd
    , init
    , onCardChange
    , onCardLoad
    , remove
    , resize
    , setValues
    , swap
    , toggleFreeSquare
    )

import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Card.TextBox as TextBox
import Bingo.Model exposing (..)
import Bingo.Page as Pages
import Bingo.Utils as Utils
import List.Extra as List


init : Card
init =
    { name = "Bingo Card"
    , values = []
    , layout =
        { size = 5
        , free = True
        }
    , style = Nothing
    }


onCardLoad : Pages.Page -> Cmd msg
onCardLoad page =
    TextBox.render (Pages.card page)


onCardChange : Pages.Page -> Cmd msg
onCardChange page =
    Cmd.batch
        [ onCardLoad page
        , Pages.save page
        ]


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


addMany : List Value -> Card -> Card
addMany toAdd card =
    let
        values =
            card.values ++ toAdd
    in
    setValues (values |> List.unique) card


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
