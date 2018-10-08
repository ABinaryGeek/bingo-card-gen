module Bingo.Card.Load exposing (decodeCard)

import Bingo.Card.Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Model exposing (Value)
import Json.Decode as Json


decodeCard : Json.Value -> Result String Card
decodeCard =
    Json.decodeValue cardDecoder >> Result.mapError Json.errorToString


cardDecoder : Json.Decoder Card
cardDecoder =
    Json.map3 Card
        (Json.field "name" Json.string)
        (Json.field "values" (Json.list valueDecoder))
        (Json.field "layout" layoutDecoder)


valueDecoder : Json.Decoder Value
valueDecoder =
    Json.string


layoutDecoder : Json.Decoder Layout
layoutDecoder =
    Json.map2 Layout
        (Json.field "size" Json.int)
        (Json.field "free" Json.bool)
