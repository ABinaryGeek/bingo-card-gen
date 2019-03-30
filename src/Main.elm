module Main exposing (main)

import Bingo.BaseUrl exposing (BaseUrl)
import Bingo.Config as Config exposing (Config)
import Bingo.Editor as Editor
import Bingo.Editor.Model exposing (Editor)
import Bingo.Errors as Errors exposing (Errors)
import Bingo.Messages exposing (..)
import Bingo.Page as Page exposing (Page)
import Bingo.Utils as Utils
import Bingo.Viewer as Viewer
import Bingo.Viewer.Messages as Viewer
import Bingo.Viewer.Model exposing (Viewer)
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Json.Encode as Json
import Url exposing (Url)


type PageModel
    = E Editor
    | V Viewer


type alias Model =
    { page : PageModel
    , reference : Maybe Page.Reference
    , key : Navigation.Key
    , errors : Errors
    , baseUrl : BaseUrl
    , config : Config
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


init : Maybe Json.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        potentialPage =
            Maybe.map Page.processFlags flags

        ( page, cmd ) =
            case potentialPage of
                Just (Page.Loaded p) ->
                    load p

                _ ->
                    let
                        ( editor, editorCmd ) =
                            Editor.init
                    in
                    ( E editor, editorCmd )

        ( referenceError, reference ) =
            Page.referenceFromUrl url |> errorAndReference

        almostModel =
            { page = page
            , key = key
            , reference = reference
            , errors = Errors.init
            , baseUrl =
                { origin =
                    Url.toString
                        { protocol = url.protocol
                        , host = url.host
                        , port_ = url.port_
                        , path = ""
                        , query = Nothing
                        , fragment = Nothing
                        }
                , path = String.split "/" url.path |> List.filter ((/=) "")
                }
            , config = Config.init
            }

        flagsError =
            case potentialPage of
                Just (Page.Saved _) ->
                    Just "Flags contained encoded data."

                Just (Page.Error message) ->
                    Just message

                _ ->
                    Nothing

        errors =
            [ flagsError, referenceError ]

        model =
            { almostModel | errors = Errors.addMany (List.filterMap identity errors) almostModel.errors }
    in
    ( model, Cmd.batch [ cmd, Config.load LoadConfig ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Page.subscriptions |> Sub.map PageMsg


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    LinkFollowed urlRequest


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChange url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditMsg msgEditor ->
            case model.page of
                E editor ->
                    case Editor.update model.config model.baseUrl model.reference msgEditor editor of
                        ( newEditor, backMessage, editorMsg ) ->
                            let
                                errors =
                                    case backMessage of
                                        Error message ->
                                            Errors.add message model.errors

                                        _ ->
                                            model.errors
                            in
                            ( { model
                                | page = E newEditor
                                , errors = errors
                              }
                            , Cmd.map EditMsg editorMsg
                            )

                V _ ->
                    ( model, Cmd.none )

        ViewMsg msgViewer ->
            case model.page of
                E _ ->
                    ( model, Cmd.none )

                V viewer ->
                    case Viewer.update model.config model.baseUrl model.reference msgViewer viewer of
                        ( newViewer, backMessage, viewerMsg ) ->
                            let
                                errors =
                                    case backMessage of
                                        Error message ->
                                            Errors.add message model.errors

                                        _ ->
                                            model.errors
                            in
                            ( { model
                                | page = V newViewer
                                , errors = errors
                              }
                            , Cmd.map ViewMsg viewerMsg
                            )

        UrlChange url ->
            onChangeUrl url model

        LinkFollowed urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    onChangeUrl url model

                Browser.External url ->
                    ( model, Navigation.load url )

        LoadConfig result ->
            case result of
                Ok config ->
                    ( { model | config = config }, Cmd.none )

                Err httpError ->
                    let
                        message =
                            "Error while getting configuration: \n" ++ Utils.httpErrorToString httpError
                    in
                    ( { model | errors = Errors.add message model.errors }, Cmd.none )

        PageMsg pageMsg ->
            case pageMsg of
                Page.Loaded page ->
                    let
                        ( newPage, cmd ) =
                            load page
                    in
                    ( { model | page = newPage }, cmd )

                Page.Saved reference ->
                    if model.reference /= Just reference then
                        ( { model | reference = Just reference }
                        , Navigation.pushUrl model.key (Page.url (Just reference))
                        )

                    else
                        ( model, Cmd.none )

                Page.Error message ->
                    ( { model | errors = Errors.add ("Error handling url: " ++ message) model.errors }, Cmd.none )

        ErrorMsg errorMsg ->
            ( { model | errors = Errors.update errorMsg model.errors }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        ( title, content ) =
            case model.page of
                E editor ->
                    ( "Editing: " ++ editor.card.name
                    , Editor.view model.baseUrl model.config model.reference editor |> List.map (Html.map EditMsg)
                    )

                V viewer ->
                    ( viewer.stampedCard.card.name
                    , [ Viewer.view model.reference viewer |> Html.map ViewMsg ]
                    )
    in
    { title = title
    , body =
        List.concat
            [ content
            , Errors.view model.errors |> List.map (Html.map ErrorMsg)
            ]
    }


onChangeUrl : Url -> Model -> ( Model, Cmd Msg )
onChangeUrl url oldModel =
    let
        ( error, reference ) =
            Page.referenceFromUrl url |> errorAndReference

        model =
            { oldModel
                | reference = reference
                , errors = Errors.maybeAdd error oldModel.errors
            }
    in
    case reference of
        Just code ->
            if oldModel.reference /= reference then
                ( model, Page.load code )

            else
                ( model, Cmd.none )

        Nothing ->
            case Editor.init of
                ( editor, editorMsg ) ->
                    ( { model | page = E editor }
                    , Cmd.map EditMsg editorMsg
                    )


fromViewer : Model -> ( Viewer, Cmd Viewer.Msg ) -> ( Model, Cmd Msg )
fromViewer model pair =
    let
        ( viewer, editorMsg ) =
            pair
    in
    ( { model | page = V viewer }
    , Cmd.map ViewMsg editorMsg
    )


load : Page -> ( PageModel, Cmd Msg )
load pageRequest =
    case pageRequest of
        Page.Edit card ->
            let
                ( editor, cmd ) =
                    Editor.load card
            in
            ( E editor, cmd )

        Page.View stampedCard ->
            let
                ( viewer, cmd ) =
                    Viewer.load stampedCard
            in
            ( V viewer, cmd |> Cmd.map ViewMsg )


errorAndReference : Maybe (Result String Page.Reference) -> ( Maybe String, Maybe Page.Reference )
errorAndReference maybeResult =
    case maybeResult of
        Just result ->
            case result of
                Ok reference ->
                    ( Nothing, Just reference )

                Err error ->
                    ( Just error, Nothing )

        Nothing ->
            ( Nothing, Nothing )
