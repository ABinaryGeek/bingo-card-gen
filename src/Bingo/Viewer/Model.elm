module Bingo.Viewer.Model exposing (Rotation, Viewer)

import Bingo.Card.Code as Code
import Bingo.Card.Model as Card exposing (Card)
import Bingo.Errors as Errors exposing (Errors)
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)


type alias Rotation =
    Int


type alias Viewer =
    { stampedCard : Card.Stamped
    , rotations : List Rotation
    }
