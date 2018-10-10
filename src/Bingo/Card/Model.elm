module Bingo.Card.Model exposing (Card, Square(..), Stamped)

import Bingo.Card.Layout exposing (Layout)
import Bingo.Model exposing (Value)
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)


type alias Card =
    { name : String
    , values : List String
    , layout : Layout
    }


type Square
    = Filled Value
    | Unfilled
    | Free


type alias Stamped =
    { card : Card
    , stamps : Stamps
    }
