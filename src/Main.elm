module Main exposing (main)

import Bingo.Card as Card
import Bingo.Card.Model exposing (Card)
import Bingo.Editor as Editor
import Bingo.Editor.Messages as Editor
import Bingo.Editor.Model exposing (Editor)
import Bingo.Viewer as Viewer
import Bingo.Viewer.Messages as Viewer
import Bingo.Viewer.Model exposing (Viewer)
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attr
import Html5.DragDrop as DragDrop
import Url exposing (Url)


type Page
    = E Editor
    | V Viewer


type Msg
    = Edit Card
    | View Card
    | EditMsg Editor.Msg
    | ViewMsg Viewer.Msg


type alias Model =
    { page : Page
    , key : Navigation.Key
    }


type alias Flags =
    {}


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
    ( { page = E (Editor.init Card.init)
      , key = key
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    Edit Card.init


onUrlChange : Url -> Msg
onUrlChange url =
    Edit Card.init


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit card ->
            ( { model | page = E (Editor.init card) }, Cmd.none )

        View card ->
            ( { model | page = V (Viewer.init card) }, Cmd.none )

        EditMsg msgEditor ->
            case model.page of
                E editor ->
                    let
                        ( newEditor, editorMsg ) =
                            Editor.update msgEditor editor
                    in
                    ( { model | page = E newEditor }, Cmd.map EditMsg editorMsg )

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
