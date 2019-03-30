module Bingo.Viewer.Model exposing (Rotation, Viewer)

import Bingo.Card.Model as Card exposing (Card)


type alias Rotation =
    Int


type alias Viewer =
    { stampedCard : Card.Stamped
    , rotations : List Rotation
    }
