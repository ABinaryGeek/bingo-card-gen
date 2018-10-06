port module Main exposing (main)

import Bingo.Card as Card
import Bingo.Card.Load as Card
import Bingo.Card.Model exposing (Card)
import Bingo.Card.Save as Card
import Bingo.Editor as Editor
import Bingo.Editor.Messages as Editor
import Bingo.Editor.Model exposing (Editor)
import Bingo.Viewer as Viewer
import Bingo.Viewer.Messages as Viewer
import Bingo.Viewer.Model exposing (Viewer)
import Browser
import Browser.Navigation as Navigation
import Debug
import Html exposing (Html)
import Html.Attributes as Attr
import Html5.DragDrop as DragDrop
import Json.Encode
import Url exposing (Url)
import Url.Builder


port decompress : String -> Cmd msg


port compress : String -> Cmd msg


port decompressed : (String -> msg) -> Sub msg


port compressed : (String -> msg) -> Sub msg


port save : String -> Cmd msg


type Page
    = E Editor
    | V Viewer


type Msg
    = NoOp
    | Compress String
    | Decompress String
    | EditMsg Editor.Msg
    | ViewMsg Viewer.Msg


type alias Model =
    { page : Page
    , key : Navigation.Key
    }


type alias Flags =
    { initial : Maybe String }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        card =
            flags.initial
                |> Maybe.andThen loadCard
                |> Maybe.withDefault Card.init
    in
    ( { page = E (Editor.init url.fragment card)
      , key = key
      }
    , Cmd.none
    )


loadCard : String -> Maybe Card
loadCard card =
    card |> Card.load |> Result.toMaybe


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ compressed (EditMsg << Editor.UpdateCode)
        , decompressed (\raw -> Editor.Load (loadCard raw |> Maybe.withDefault Card.init) |> EditMsg)
        ]


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    url.fragment |> Maybe.map (\value -> Decompress value) |> Maybe.withDefault NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditMsg msgEditor ->
            case model.page of
                E editor ->
                    let
                        ( newEditor, editorMsg ) =
                            Editor.update msgEditor editor

                        updateCode =
                            if newEditor.card /= editor.card then
                                [ compress (newEditor.card |> Card.save) ]

                            else
                                []

                        changeUrl =
                            case msgEditor of
                                Editor.UpdateCode code ->
                                    [ codeUrl code |> Navigation.pushUrl model.key ]

                                Editor.Save ->
                                    [ save editor.card.name ]

                                _ ->
                                    []
                    in
                    ( { model | page = E newEditor }
                    , Cmd.batch
                        (List.concat
                            [ [ Cmd.map EditMsg editorMsg ]
                            , updateCode
                            , changeUrl
                            ]
                        )
                    )

                V viewer ->
                    ( model, Cmd.none )

        ViewMsg msgViewer ->
            case model.page of
                E editor ->
                    ( model, Cmd.none )

                V viewer ->
                    let
                        ( newViewer, viewerMsg ) =
                            Viewer.update msgViewer viewer
                    in
                    ( { model | page = V newViewer }, Cmd.map ViewMsg viewerMsg )

        Compress raw ->
            ( model, compress raw )

        Decompress compact ->
            ( model, decompress compact )

        NoOp ->
            ( model, Cmd.none )


codeUrl : String -> String
codeUrl code =
    Url.Builder.custom Url.Builder.Relative [] [] (Just code)


view : Model -> Browser.Document Msg
view model =
    case model.page of
        E editor ->
            { title = "Bingo Card Creator - Edit"
            , body = [ Editor.view editor |> Html.map EditMsg ]
            }

        V viewer ->
            { title = viewer.card.name
            , body = [ Viewer.view viewer |> Html.map ViewMsg ]
            }
