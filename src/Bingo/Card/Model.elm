module Bingo.Card.Model exposing (Card, Square(..), Stamped, Style, defaultStyle, getColor)

import Bingo.Card.Layout exposing (Layout)
import Bingo.Model exposing (Value)
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)
import Color exposing (Color)
import Color.Hex as Color


type alias Card =
    { name : String
    , values : List String
    , layout : Layout
    , style : Maybe Style
    }


type Square
    = Filled Value
    | Unfilled
    | Free


type alias Stamped =
    { card : Card
    , stamps : Stamps
    }


type alias Style =
    { title : Color
    , background : Color
    }


defaultStyle : Style
defaultStyle =
    { title = Color.rgb255 170 120 240
    , background = Color.rgb255 240 205 120
    }


getColor : (Style -> Color) -> Maybe Style -> Color
getColor getter style =
    style |> Maybe.map getter |> Maybe.withDefault (getter defaultStyle)
