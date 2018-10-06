module Bingo.Viewer exposing
    ( init
    , update
    , view
    )

import Bingo.Card
import Bingo.Card.Model exposing (Card)
import Bingo.Viewer.Messages exposing (..)
import Bingo.Viewer.Model exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


init : Card -> Viewer
init card =
    { card = card }


view : Viewer -> Html Msg
view model =
    Html.div [] []


update : Msg -> Viewer -> ( Viewer, Cmd Msg )
update msg model =
    ( model, Cmd.none )
