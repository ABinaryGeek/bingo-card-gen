module Bingo.Editor exposing
    ( init
    , load
    , update
    , view
    )

import Bingo.BaseUrl as BaseUrl exposing (BaseUrl)
import Bingo.Card as Card
import Bingo.Card.Code as Code
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Card.TextBox as TextBox
import Bingo.Card.View as Card
import Bingo.Config as Config exposing (Config)
import Bingo.Editor.ImportOverlay as ImportOverlay exposing (ImportOverlay)
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Editor.ValueList as ValueList
import Bingo.Errors as Errors exposing (Errors)
import Bingo.Icon as Icon
import Bingo.Messages as Messages
import Bingo.Model exposing (..)
import Bingo.Page as Page exposing (Page)
import Bingo.Save as Save
import Bingo.ShortUrl as ShortUrl exposing (ShortUrl)
import Bingo.Utils as Utils
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Html
import Html5.DragDrop as DragDrop
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Task


init : Code.Out msg -> TextBox.TextBoxesOut msg -> ( Editor, Cmd msg )
init codeOut textBoxesOut =
    let
        editor =
            emptyEditor
    in
    ( editor, Card.onCardChange codeOut textBoxesOut (Page.Edit editor.card) )


load : Code.Out msg -> TextBox.TextBoxesOut msg -> Card -> ( Editor, Cmd msg )
load codeOut textBoxesOut card =
    ( emptyEditorWithCard card, Card.onCardLoad textBoxesOut (Page.Edit card) )


update : Save.Out Msg -> Code.Out Msg -> TextBox.TextBoxesOut Msg -> Navigation.Key -> Config -> BaseUrl -> Maybe Page.Reference -> Msg -> Editor -> ( Editor, Messages.Back, Cmd Msg )
update saveOut codeOut textBoxesOut key config baseUrl reference msg model =
    let
        ( editor, backMessage, cmd ) =
            internalUpdate saveOut key config baseUrl reference msg model

        card =
            editor.card

        ( newEditor, commands ) =
            if card /= model.card then
                ( { editor | editShortUrl = ShortUrl.Unknown, viewShortUrl = ShortUrl.Unknown }
                , Cmd.batch [ cmd, Card.onCardChange codeOut textBoxesOut (Page.Edit card) ]
                )

            else
                ( editor, cmd )
    in
    ( { newEditor | card = card }, backMessage, commands )


view : BaseUrl -> Config -> Maybe Page.Reference -> Editor -> List (Html Msg)
view baseUrl config reference model =
    let
        dropTarget =
            DragDrop.getDropId model.dragDrop

        attributes =
            squareAttributes
                (Maybe.andThen
                    (\t ->
                        case t of
                            ValueTarget value ->
                                Just value

                            _ ->
                                Nothing
                    )
                    dropTarget
                )
    in
    List.concat
        [ [ Html.div [ Attr.class "editor" ]
                [ Html.div [ Attr.class "container big" ]
                    [ Html.a
                        [ Attr.href (reference |> Maybe.map Page.referenceAsView |> Page.url)
                        , Attr.target "_blank"
                        ]
                        [ Card.view model.card ]
                    ]
                , Html.div
                    [ Attr.class "controls" ]
                    ([ ValueList.view attributes model.card (drag model.dragDrop)
                     , Html.div [ Attr.class "container" ]
                        [ ValueList.add model.card model.newValueInput
                        , settings model.card
                        , share baseUrl reference config.urlShortener model.viewShortUrl model.editShortUrl
                        ]
                     ]
                        ++ ValueList.commonlyAdded config.commonValues model.card
                    )
                ]
          ]
        , [ Html.a
                [ Attr.href "https://github.com/ABinaryGeek/bingo-card-gen"
                , Attr.class "github-ribbon"
                ]
                [ Icon.github
                , Html.text " Fork me on GitHub"
                ]
          ]
        , ImportOverlay.view model.importOverlay |> List.map (Html.map ImportOverlayMsg)
        ]



{- Private -}


emptyEditor : Editor
emptyEditor =
    emptyEditorWithCard Card.init


emptyEditorWithCard : Card -> Editor
emptyEditorWithCard card =
    { card = card
    , newValueInput = ""
    , importOverlay = ImportOverlay.init
    , viewShortUrl = ShortUrl.Unknown
    , editShortUrl = ShortUrl.Unknown
    , dragDrop = DragDrop.init
    }


internalUpdate : Save.Out Msg -> Navigation.Key -> Config -> BaseUrl -> Maybe Page.Reference -> Msg -> Editor -> ( Editor, Messages.Back, Cmd Msg )
internalUpdate saveOut key config baseUrl reference msg model =
    case msg of
        AddNewValue ->
            ( { model
                | card = Card.add model.newValueInput model.card
                , newValueInput = ""
              }
            , Messages.NoBackMessage
            , Task.attempt (\_ -> NoOp) (Dom.focus "add-value-field")
            )

        AddGivenValue givenValue ->
            ( { model | card = Card.add givenValue model.card }, Messages.NoBackMessage, Cmd.none )

        UpdateNewValueField newValue ->
            ( { model | newValueInput = newValue }, Messages.NoBackMessage, Cmd.none )

        ChangeName name ->
            ( { model | card = Card.changeName name model.card }, Messages.NoBackMessage, Cmd.none )

        Resize sizeString ->
            let
                card =
                    case String.toInt sizeString of
                        Just size ->
                            Card.resize size model.card

                        Nothing ->
                            model.card
            in
            ( { model
                | card = card
              }
            , Messages.NoBackMessage
            , Cmd.none
            )

        ToggleFreeSquare ->
            ( { model | card = Card.toggleFreeSquare model.card }, Messages.NoBackMessage, Cmd.none )

        Randomise { includeUnused } ->
            if includeUnused then
                ( model, Messages.NoBackMessage, Random.generate Reorder (Random.List.shuffle model.card.values) )

            else
                let
                    amount =
                        Layout.amountOfValues model.card.layout

                    ( used, unused ) =
                        Utils.split amount model.card.values
                in
                ( model
                , Messages.NoBackMessage
                , Random.generate Reorder
                    (Random.List.shuffle used
                        |> Random.map (\shuffled -> shuffled ++ unused)
                    )
                )

        Reorder values ->
            ( { model | card = Card.setValues values model.card }, Messages.NoBackMessage, Cmd.none )

        Save ->
            ( model, Messages.NoBackMessage, Save.save saveOut model.card.name )

        Shorten target ->
            case config.urlShortener of
                Just urlShortener ->
                    ( updateShortUrl target ShortUrl.Requested model
                    , Messages.NoBackMessage
                    , ShortUrl.request urlShortener baseUrl reference target |> Cmd.map ShortUrlMsg
                    )

                Nothing ->
                    ( model, Messages.NoBackMessage, Cmd.none )

        NoOp ->
            ( model, Messages.NoBackMessage, Cmd.none )

        ShortUrlMsg (ShortUrl.Response target response) ->
            case response of
                Ok shortUrl ->
                    ( updateShortUrl target (ShortUrl.Known shortUrl) model, Messages.NoBackMessage, Cmd.none )

                Err error ->
                    ( updateShortUrl target ShortUrl.Unknown model
                    , Messages.Error ("Error getting short url: " ++ Utils.httpErrorToString error)
                    , Cmd.none
                    )

        ImportOverlayMsg importOverlayMsg ->
            let
                ( importOverlay, result ) =
                    ImportOverlay.update importOverlayMsg model.importOverlay

                toAdd =
                    case result of
                        ImportOverlay.Import values ->
                            values

                        ImportOverlay.NoResult ->
                            []
            in
            ( { model
                | importOverlay = importOverlay
                , card = Card.addMany toAdd model.card
              }
            , Messages.NoBackMessage
            , Cmd.none
            )

        DragDropMsg dragDropMsg ->
            let
                ( dragDrop, result ) =
                    DragDrop.update dragDropMsg model.dragDrop

                card =
                    case result of
                        Just ( dragged, dropTarget, position ) ->
                            case dropTarget of
                                ValueTarget drop ->
                                    Card.swap dragged drop model.card

                                BinTarget ->
                                    Card.remove dragged model.card

                        Nothing ->
                            model.card
            in
            ( { model
                | dragDrop = dragDrop
                , card = card
              }
            , Messages.NoBackMessage
            , Cmd.none
            )


updateShortUrl : ShortUrlTarget -> ShortUrl -> Editor -> Editor
updateShortUrl target value model =
    case target of
        EditShortUrl ->
            { model | editShortUrl = value }

        ViewShortUrl ->
            { model | viewShortUrl = value }


drag : DragDrop.Model Value DropTarget -> Maybe Drag
drag dragDrop =
    case DragDrop.getDragId dragDrop of
        Just draggedValue ->
            Just
                { draggedValue = draggedValue
                , hoveredValue = DragDrop.getDropId dragDrop
                }

        Nothing ->
            Nothing


squareAttributes : Maybe Value -> Value -> List (Html.Attribute Msg)
squareAttributes highlighted name =
    dragDropTarget name ++ highlightedClass name highlighted


settings : Card -> Html Msg
settings card =
    Html.div []
        [ Html.h2 [] [ Html.text "Settings" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ changeNameControl card.name
            , changeSizeControl card.layout
            , freeSquareControl card.layout
            ]
        ]


share : BaseUrl -> Maybe Page.Reference -> Maybe String -> ShortUrl -> ShortUrl -> Html Msg
share baseUrl reference urlShortener viewShortUrl editShortUrl =
    Html.div []
        [ Html.h2 [] [ Html.text "Share" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            (reference
                |> Maybe.map
                    (\r ->
                        [ linkView viewShortUrl baseUrl urlShortener r
                        , linkEdit editShortUrl baseUrl urlShortener r
                        , saveImage
                        ]
                    )
                |> Maybe.withDefault []
            )
        ]


linkEdit : ShortUrl -> BaseUrl -> Maybe String -> Page.Reference -> Html Msg
linkEdit shortUrl baseUrl urlShortener reference =
    copyableLink EditShortUrl shortUrl "Link To Edit: " "edit-link-field" baseUrl urlShortener (Page.referenceAsEdit reference)


linkView : ShortUrl -> BaseUrl -> Maybe String -> Page.Reference -> Html Msg
linkView shortUrl baseUrl urlShortener reference =
    copyableLink ViewShortUrl shortUrl "Link To View: " "view-link-field" baseUrl urlShortener (Page.referenceAsView reference)


copyableLink : ShortUrlTarget -> ShortUrl -> String -> String -> BaseUrl -> Maybe String -> Page.Reference -> Html Msg
copyableLink shortUrlTarget shortUrl description id baseUrl urlShortener reference =
    let
        longUrl =
            Page.externalUrl baseUrl (Just reference)

        ( disabled, icon, url ) =
            case shortUrl of
                ShortUrl.Unknown ->
                    ( False, Icon.cut, longUrl )

                ShortUrl.Requested ->
                    ( True, Icon.cog, "Generating..." )

                ShortUrl.Known sUrl ->
                    ( True, Icon.check, sUrl )

        shortenButton =
            if urlShortener /= Nothing then
                [ Html.button
                    [ Attr.class "shorten-button pure-button"
                    , Html.onClick (Shorten shortUrlTarget)
                    , Attr.disabled disabled
                    , Attr.title "Get a short link for this card."
                    ]
                    [ icon ]
                ]

            else
                []
    in
    Html.div [ Attr.class "pure-control-group" ]
        [ Html.label [ Attr.for id ] [ Html.text description ]
        , Html.div [ Attr.class "form-row" ]
            ([ Html.input
                [ Attr.id id
                , Attr.class "pure-input-1"
                , Attr.value url
                , Attr.readonly True
                ]
                []
             , Html.button
                [ Attr.class "copy-button pure-button"
                , Attr.attribute "data-clipboard-target" ("#" ++ id)
                , Attr.title "Copy the link to this card."
                ]
                [ Icon.copy ]
             ]
                ++ shortenButton
            )
        ]


saveImage : Html Msg
saveImage =
    Html.div [ Attr.class "pure-control-group" ]
        [ Html.label [ Attr.for "save-button" ] [ Html.text "Save " ]
        , Html.button
            [ Html.onClick Save
            , Attr.id "save-button"
            , Attr.class "pure-button"
            ]
            [ Icon.image, Html.text " As Image" ]
        ]


changeNameControl : String -> Html Msg
changeNameControl name =
    Html.div [ Attr.class "change-name pure-control-group" ]
        [ Html.label [ Attr.for "name-field" ] [ Html.text "Name: " ]
        , Html.input
            [ Attr.id "name-field"
            , Attr.class "pure-input-1"
            , Attr.value name
            , Html.onInput ChangeName
            ]
            []
        ]


changeSizeControl : Layout -> Html Msg
changeSizeControl layout =
    Html.div [ Attr.class "pure-control-group" ]
        [ Html.label [ Attr.for "size-field" ] [ Html.text "Width/Height: " ]
        , Html.input
            [ Attr.id "size-field"
            , Attr.class "pure-input-1"
            , Attr.type_ "number"
            , Attr.value (String.fromInt layout.size)
            , Html.onInput Resize
            ]
            []
        , Html.span [ Attr.class "pure-form-message-inline" ]
            (List.map
                Html.text
                [ "(="
                , String.fromInt (Layout.amountOfValues layout)
                , " Values)"
                ]
            )
        ]


freeSquareControl : Layout -> Html Msg
freeSquareControl layout =
    Html.div [ Attr.class "free-square pure-control-group" ]
        [ Html.label [ Attr.for "free-square-field" ] [ Html.text " Free Square: " ]
        , Html.input
            [ Attr.id "free-square-field"
            , Attr.type_ "checkbox"
            , Attr.class "pure-checkbox"
            , Html.onClick ToggleFreeSquare
            , Attr.checked (Layout.freeSquareValid layout && layout.free)
            , Attr.disabled (not (Layout.freeSquareValid layout))
            ]
            []
        ]


highlightedClass : Value -> Maybe Value -> List (Html.Attribute msg)
highlightedClass value target =
    if Just value == target then
        [ Attr.class "hovered" ]

    else
        []


dragDropTarget : Value -> List (Html.Attribute Msg)
dragDropTarget value =
    List.concat
        [ DragDrop.draggable DragDropMsg value
        , DragDrop.droppable DragDropMsg (ValueTarget value)
        , [ Attr.attribute "ondragstart" "" ]
        ]
