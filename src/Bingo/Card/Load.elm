module Bingo.Card.Load exposing (load)

import Base64
import Bingo.Card.Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Model exposing (Value)
import Json.Decode as Json
import UrlBase64


b64d : String -> Result String String
b64d =
    UrlBase64.decode Base64.decode


load : String -> Result String Card
load card =
    b64d card
        |> Result.andThen
            (Json.decodeString decodeCard >> Result.mapError Json.errorToString)


decodeCard : Json.Decoder Card
decodeCard =
    Json.map3 Card
        (Json.field "name" Json.string)
        (Json.field "values" (Json.list decodeValue))
        (Json.field "layout" decodeLayout)


decodeValue : Json.Decoder Value
decodeValue =
    Json.string


decodeLayout : Json.Decoder Layout
decodeLayout =
    Json.map2 Layout
        (Json.field "size" Json.int)
        (Json.field "free" Json.bool)
