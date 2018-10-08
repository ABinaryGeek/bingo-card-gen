port module Bingo.Card.Code exposing
    ( CodeIn
    , CodeOut
    , CompressedCode
    , Msg(..)
    , RawCode
    , loadCard
    , saveCard
    , subscriptions
    )

import Bingo.Card.Load as Load
import Bingo.Card.Model exposing (Card)
import Bingo.Card.Save as Save
import Bingo.Utils as Utils
import Json.Decode as Json
import Json.Encode


{-| The type of the inbound port required.
-}
type alias CodeIn =
    (Json.Value -> Msg) -> Sub Msg


{-| The type of the outbound port required.
-}
type alias CodeOut msg =
    Json.Value -> Cmd msg


{-| A serialized compressed URL-encoded text string representation of a card.
-}
type alias CompressedCode =
    String


{-| A serialised JSON representation of a card.
-}
type alias RawCode =
    Json.Value


{-| Messages as a result of save/load commands.
-}
type Msg
    = Loaded Card
    | Saved CompressedCode
    | Error String


{-| Save a card to a compressed code. Takes the port for doing this as the
first argument. Will result in a Saved or Error message.
-}
saveCard : CodeOut msg -> Card -> Cmd msg
saveCard outPort card =
    toCode card |> Raw |> encodeCode |> outPort


{-| Load a card from a compressed code. Takes the port for doing this as the
first argument. Will result in a Loaded or Error message.
-}
loadCard : CodeOut msg -> CompressedCode -> Cmd msg
loadCard outPort code =
    code |> Compressed |> encodeCode |> outPort


{-| This subscription allows you to listen for responses from the port with
the result of your requests.
-}
subscriptions : CodeIn -> Sub Msg
subscriptions inPort =
    inPort (decodeCode >> inboundCode)



{--Private--}


type Code
    = Raw RawCode
    | Compressed CompressedCode


inboundCode : Result String Code -> Msg
inboundCode code =
    case code of
        Ok (Raw rawCode) ->
            case Load.decodeCard rawCode of
                Ok card ->
                    Loaded card

                Err error ->
                    Error error

        Ok (Compressed compressedCode) ->
            Saved compressedCode

        Err error ->
            Error error


toCode : Card -> RawCode
toCode card =
    Save.encodeCard card


decodeCode : Json.Value -> Result String Code
decodeCode =
    Json.decodeValue codeDecoder >> Result.mapError Json.errorToString


codeDecoder : Json.Decoder Code
codeDecoder =
    errorDecoder
        |> Json.andThen
            (\error ->
                case error of
                    Just message ->
                        Json.fail message

                    Nothing ->
                        Json.field "compressed" Json.bool
                            |> Json.andThen
                                (\compressed ->
                                    if compressed then
                                        Json.field "code" Json.string |> Json.map Compressed

                                    else
                                        Json.field "code" Json.value |> Json.map Raw
                                )
            )


errorDecoder : Json.Decoder (Maybe String)
errorDecoder =
    Json.field "error" Json.string |> Json.maybe


encodeCode : Code -> Json.Value
encodeCode code =
    case code of
        Raw rawCode ->
            Json.Encode.object
                [ ( "compressed", Json.Encode.bool False )
                , ( "code", rawCode )
                ]

        Compressed compressedCode ->
            Json.Encode.object
                [ ( "compressed", Json.Encode.bool True )
                , ( "code", Json.Encode.string compressedCode )
                ]
