module Bingo.Config exposing (Config, init, load)

import Bingo.Model exposing (..)
import Http
import Json.Decode as Json


type alias Config =
    { urlShortener : Maybe String
    , commonValues : List Value
    }


init : Config
init =
    { urlShortener = Nothing
    , commonValues = []
    }


load : (Result Http.Error Config -> msg) -> Cmd msg
load handle =
    Http.get { url = "config.json", expect = Http.expectJson handle configDecoder }



{- Private -}


configDecoder : Json.Decoder Config
configDecoder =
    Json.map2 Config
        (Json.maybe (Json.field "urlShortener" <| Json.string))
        (Json.field "commonValues" <| Json.list Json.string)
