module Bingo.Card.Save exposing (save)

import Base64
import Bingo.Card.Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Model exposing (Value)
import Json.Encode as Json
import UrlBase64


b64e : String -> Result String String
b64e =
    UrlBase64.encode (Base64.encode >> Result.Ok)


save : Card -> String
save card =
    encodeCard card |> Json.encode 0 |> b64e |> Result.withDefault ""


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
