module Bingo.Card.Code exposing
    ( Compressed
    , In
    , Msg(..)
    , Out
    , decode
    , encode
    , processFlags
    , subscriptions
    )

import Bingo.Card.Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Model exposing (Value)
import Bingo.Utils as Utils
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)
import Color exposing (Color)
import Color.Hex as Color
import Json.Decode as Json
import Json.Encode
import Url.Builder


{-| The type of the inbound port required.
-}
type alias In =
    (Json.Value -> Msg) -> Sub Msg


{-| The type of the outbound port required.
-}
type alias Out msg =
    Json.Value -> Cmd msg


{-| Messages as a result of encode/decode commands.
-}
type Msg
    = Encoded Compressed (Maybe Json.Value)
    | Decoded Card (Maybe Json.Value)
    | Error String


{-| A serialized compressed URL-encoded text string representation of a card.
-}
type alias Compressed =
    String


{-| This subscription allows you to listen for responses from the port with
the result of your requests.
-}
subscriptions : In -> Sub Msg
subscriptions inPort =
    inPort (decodeCode >> inboundCode)


{-| Encode a card, with some extra information to go with/identify the code when it is returned.
-}
encode : Out msg -> Card -> Maybe Json.Value -> Cmd msg
encode outPort card sideCar =
    RawCode (toCode card) sideCar |> encodeCode |> outPort


{-| Decode a card, with any extra information to go with/identify the card when it is returned.
-}
decode : Out msg -> Compressed -> Maybe Json.Value -> Cmd msg
decode outPort code sideCar =
    CompressedCode code sideCar |> encodeCode |> outPort


{-| Process some data as though it were a subscription, useful for flags.
-}
processFlags : Json.Value -> Msg
processFlags value =
    decodeCode value |> inboundCode



{--Private--}


{-| A serialised JSON representation of a card.
-}
type alias Raw =
    Json.Value


type Code
    = RawCode Raw (Maybe Json.Value)
    | CompressedCode Compressed (Maybe Json.Value)


inboundCode : Result String Code -> Msg
inboundCode code =
    case code of
        Ok (RawCode rawCode sideCar) ->
            case Json.decodeValue cardDecoder rawCode |> Result.mapError Json.errorToString of
                Ok card ->
                    Decoded card sideCar

                Err error ->
                    Error error

        Ok (CompressedCode compressedCode sideCar) ->
            Encoded compressedCode sideCar

        Err error ->
            Error error


toCode : Card -> Raw
toCode card =
    encodeCard card


decodeCode : Json.Value -> Result String Code
decodeCode =
    Json.decodeValue codeDecoder >> Result.mapError Json.errorToString >> Utils.flattenResult


codeDecoder : Json.Decoder (Result String Code)
codeDecoder =
    errorWrappedDecoder (Json.field "compressed" Json.bool |> Json.andThen internalCodeDecoder)


internalCodeDecoder : Bool -> Json.Decoder Code
internalCodeDecoder compressed =
    let
        maybeSideCar =
            Json.maybe (Json.field "sideCar" Json.value)
    in
    if compressed then
        Json.map2 CompressedCode
            (Json.field "code" Json.string)
            maybeSideCar

    else
        Json.map2 RawCode
            (Json.field "code" Json.value)
            maybeSideCar


errorWrappedDecoder : Json.Decoder a -> Json.Decoder (Result String a)
errorWrappedDecoder decoder =
    Json.field "error" Json.string |> Json.maybe |> Json.andThen (errorResult decoder)


errorResult : Json.Decoder a -> Maybe String -> Json.Decoder (Result String a)
errorResult decoder maybeError =
    case maybeError of
        Just error ->
            Json.succeed (Err error)

        Nothing ->
            decoder |> Json.map Ok


encodeCode : Code -> Json.Value
encodeCode code =
    let
        ( compressed, encodedCode, maybeSideCar ) =
            case code of
                RawCode rawCode mS ->
                    ( False, rawCode, mS )

                CompressedCode compressedCode mS ->
                    ( True, Json.Encode.string compressedCode, mS )

        sideCarField =
            maybeSideCar |> Maybe.map (\s -> ( "sideCar", s )) |> Utils.singletonOrEmpty
    in
    Json.Encode.object
        ([ ( "compressed", Json.Encode.bool compressed )
         , ( "code", encodedCode )
         ]
            ++ sideCarField
        )


cardDecoder : Json.Decoder Card
cardDecoder =
    Json.map4 Card
        (Json.field "name" Json.string)
        (Json.field "values" (Json.list valueDecoder))
        (Json.field "layout" layoutDecoder)
        (Json.maybe (Json.field "style" styleDecoder))


styleDecoder : Json.Decoder Style
styleDecoder =
    Json.map2 Style
        (Json.field "title" colorDecoder)
        (Json.field "background" colorDecoder)


valueDecoder : Json.Decoder Value
valueDecoder =
    Json.string


layoutDecoder : Json.Decoder Layout
layoutDecoder =
    Json.map2 Layout
        (Json.field "size" Json.int)
        (Json.field "free" Json.bool)


colorDecoder : Json.Decoder Color
colorDecoder =
    Json.string
        |> Json.map Color.fromHex
        |> Json.andThen
            (\c ->
                case c of
                    Just color ->
                        Json.succeed color

                    Nothing ->
                        Json.fail "Invalid hex color"
            )


encodeCard : Card -> Json.Value
encodeCard card =
    let
        style =
            card.style |> Maybe.map (\s -> [ ( "style", encodeStyle s ) ]) |> Maybe.withDefault []
    in
    Json.Encode.object
        ([ ( "name", Json.Encode.string <| card.name )
         , ( "values", Json.Encode.list encodeValue <| card.values )
         , ( "layout", encodeLayout <| card.layout )
         ]
            ++ style
        )


encodeValue : Value -> Json.Value
encodeValue value =
    Json.Encode.string value


encodeLayout : Layout -> Json.Value
encodeLayout layout =
    Json.Encode.object
        [ ( "size", Json.Encode.int <| layout.size )
        , ( "free", Json.Encode.bool <| layout.free )
        ]


encodeStyle : Style -> Json.Value
encodeStyle style =
    Json.Encode.object
        [ ( "title", encodeColor style.title )
        , ( "background", encodeColor style.background )
        ]


encodeColor : Color -> Json.Value
encodeColor color =
    color |> Color.toHex |> .hex |> Json.Encode.string
