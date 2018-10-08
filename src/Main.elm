port module Main exposing (main)

import Bingo.Card as Card
import Bingo.Card.Code as Code
import Bingo.Card.Load as Card
import Bingo.Card.Model exposing (Card)
import Bingo.Card.Save as Card
import Bingo.Card.TextBox as TextBox
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
import Json.Encode as Json
import Url exposing (Url)
import Url.Builder


port codeOut : Code.CodeOut msg


port codeIn : (Json.Value -> msg) -> Sub msg


port saveOut : Editor.SaveOut msg


port textBoxesOut : TextBox.TextBoxesOut msg


type Page
    = E Editor
    | V Viewer


type Msg
    = NoOp
    | UrlChange (Maybe Code.CompressedCode)
    | EditMsg Editor.Msg
    | ViewMsg Viewer.Msg


type alias Model =
    { page : Page
    , key : Navigation.Key
    }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : {} -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( editor, cmd ) =
            Editor.init codeOut textBoxesOut url.fragment
    in
    ( { page = E editor
      , key = key
      }
    , cmd |> Cmd.map EditMsg
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Editor.subscriptions codeIn |> Sub.map EditMsg


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChange url.fragment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditMsg msgEditor ->
            case model.page of
                E editor ->
                    let
                        ( newEditor, editorMsg ) =
                            Editor.update saveOut codeOut textBoxesOut model.key msgEditor editor
                    in
                    ( { model | page = E newEditor }
                    , Cmd.map EditMsg editorMsg
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

        UrlChange maybeCode ->
            case model.page of
                E editor ->
                    let
                        ( newEditor, editorMsg ) =
                            Editor.onChangeUrl codeOut textBoxesOut maybeCode editor
                    in
                    ( { model | page = E newEditor }
                    , Cmd.map EditMsg editorMsg
                    )

                V viewer ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
