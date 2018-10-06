module Bingo.Card.Model exposing (Card, Square(..))

import Bingo.Card.Layout exposing (Layout)
import Bingo.Model exposing (Value)


type alias Card =
    { name : String
    , values : List String
    , layout : Layout
    }


type Square
    = Filled Value
    | Unfilled
    | Free
