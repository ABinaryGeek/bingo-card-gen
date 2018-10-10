module Bingo.Config exposing (Config, init, load)

import Bingo.Model exposing (..)
import Http
import Json.Decode as Json


type alias Config =
    { commonValues : List Value
    }


init : Config
init =
    { commonValues = []
    }


load : (Result Http.Error Config -> msg) -> Cmd msg
load handle =
    Http.get "config.json" configDecoder |> Http.send handle



{- Private -}


configDecoder : Json.Decoder Config
configDecoder =
    Json.map Config
        (Json.field "commonValues" <| Json.list Json.string)
