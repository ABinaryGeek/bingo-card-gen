module Bingo.Viewer.Model exposing (Rotation, Viewer)

import Bingo.Card.Model as Card exposing (Card)
import Bingo.ShortUrl exposing (ShortUrl)


type alias Rotation =
    Int


type alias Viewer =
    { stampedCard : Card.Stamped
    , rotations : List Rotation
    , shortUrl : ShortUrl
    }
