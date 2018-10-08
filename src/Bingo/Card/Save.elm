module Bingo.Card.Save exposing (encodeCard)

import Bingo.Card.Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Model exposing (Value)
import Json.Encode as Json


encodeCard : Card -> Json.Value
encodeCard card =
    Json.object
        [ ( "name", Json.string <| card.name )
        , ( "values", Json.list encodeValue <| card.values )
        , ( "layout", encodeLayout <| card.layout )
        ]


encodeValue : Value -> Json.Value
encodeValue value =
    Json.string value


encodeLayout : Layout -> Json.Value
encodeLayout layout =
    Json.object
        [ ( "size", Json.int <| layout.size )
        , ( "free", Json.bool <| layout.free )
        ]
