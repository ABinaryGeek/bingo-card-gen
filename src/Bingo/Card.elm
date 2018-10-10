module Bingo.Card exposing
    ( add
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

import Bingo.Card.Code as Code
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Card.TextBox as TextBox
import Bingo.Model exposing (..)
import Bingo.Page as Pages
import Bingo.Utils as Utils
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)


init : Card
init =
    { name = "Bingo Card"
    , values = []
    , layout =
        { size = 5
        , free = True
        }
    }


onCardLoad : TextBox.TextBoxesOut msg -> Pages.Page -> Cmd msg
onCardLoad textBoxesOut page =
    TextBox.render textBoxesOut (Pages.card page)


onCardChange : Code.Out msg -> TextBox.TextBoxesOut msg -> Pages.Page -> Cmd msg
onCardChange codeOut textBoxesOut page =
    Cmd.batch
        [ onCardLoad textBoxesOut page
        , Pages.save codeOut page
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
