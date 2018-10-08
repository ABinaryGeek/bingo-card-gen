module Bingo.Editor exposing
    ( SaveOut
    , init
    , onChangeUrl
    , subscriptions
    , update
    , view
    )

import Bingo.Card as Card
import Bingo.Card.Code as Code
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Card.Save as Card
import Bingo.Card.TextBox as TextBox
import Bingo.Card.View as Card
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Editor.ValueList as ValueList
import Bingo.Icon as Icon
import Bingo.Model exposing (Value)
import Bingo.Utils as Utils
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
import Url exposing (Url)
import Url.Builder


type alias SaveOut msg =
    String -> Cmd msg


init : Code.CodeOut msg -> TextBox.TextBoxesOut msg -> Maybe Code.CompressedCode -> ( Editor, Cmd msg )
init codeOut textBoxesOut maybeCode =
    let
        ( editor, cmd ) =
            onChangeUrl codeOut textBoxesOut maybeCode emptyEditor

        -- Catch a real dumb edge case where we don't load anything if we get given a code that is
        -- the same as the default card.
        cmds =
            if editor.card == emptyEditor.card then
                Cmd.batch [ cmd, onCardChange codeOut textBoxesOut editor.card ]

            else
                cmd
    in
    ( editor, cmds )


subscriptions : Code.CodeIn -> Sub Msg
subscriptions codeIn =
    Code.subscriptions codeIn |> Sub.map CodeMsg


update : SaveOut Msg -> Code.CodeOut Msg -> TextBox.TextBoxesOut Msg -> Navigation.Key -> Msg -> Editor -> ( Editor, Cmd Msg )
update saveOut codeOut textBoxesOut key msg model =
    let
        ( editor, cmd ) =
            internalUpdate saveOut key msg model

        card =
            editor.card

        commands =
            if card /= model.card then
                Cmd.batch [ cmd, onCardChange codeOut textBoxesOut card ]

            else
                cmd
    in
    ( { editor | card = card }, commands )


view : Editor -> Html Msg
view model =
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

        errorBox =
            if List.isEmpty model.errors then
                []

            else
                [ errors model.errors ]
    in
    Html.div [ Attr.class "editor" ]
        ([ Html.div [ Attr.class "container big" ]
            [ Card.view model.card ]
         , Html.div
            [ Attr.class "controls" ]
            [ ValueList.view attributes model.card (drag model.dragDrop)
            , Html.div [ Attr.class "container" ]
                [ ValueList.add model.card model.newValueInput
                , settings model.card
                , share model.code
                ]
            , ValueList.commonlyAdded model.card
            ]
         ]
            ++ errorBox
        )


onChangeUrl : Code.CodeOut msg -> TextBox.TextBoxesOut msg -> Maybe Code.CompressedCode -> Editor -> ( Editor, Cmd msg )
onChangeUrl codeOut textBoxesOut maybeCode editor =
    case maybeCode of
        Just code ->
            if editor.code /= maybeCode then
                ( editor, Code.loadCard codeOut code )

            else
                ( editor, Cmd.none )

        Nothing ->
            internalInit codeOut textBoxesOut



{- Private -}


emptyEditor : Editor
emptyEditor =
    { code = Nothing
    , card = Card.init
    , newValueInput = ""
    , dragDrop = DragDrop.init
    , errors = []
    }


internalInit : Code.CodeOut msg -> TextBox.TextBoxesOut msg -> ( Editor, Cmd msg )
internalInit codeOut textBoxesOut =
    let
        editor =
            emptyEditor
    in
    ( editor, onCardChange codeOut textBoxesOut editor.card )


onCardChange : Code.CodeOut msg -> TextBox.TextBoxesOut msg -> Card -> Cmd msg
onCardChange codeOut textBoxesOut card =
    Cmd.batch
        [ Code.saveCard codeOut card
        , TextBox.render textBoxesOut card
        ]


internalUpdate : SaveOut Msg -> Navigation.Key -> Msg -> Editor -> ( Editor, Cmd Msg )
internalUpdate saveOut key msg model =
    case msg of
        AddNewValue ->
            ( { model
                | card = Card.add model.newValueInput model.card
                , newValueInput = ""
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "add-value-field")
            )

        AddGivenValue givenValue ->
            ( { model | card = Card.add givenValue model.card }, Cmd.none )

        UpdateNewValueField newValue ->
            ( { model | newValueInput = newValue }, Cmd.none )

        ChangeName name ->
            ( { model | card = Card.changeName name model.card }, Cmd.none )

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
            , Cmd.none
            )

        ToggleFreeSquare ->
            ( { model | card = Card.toggleFreeSquare model.card }, Cmd.none )

        Randomise { includeUnused } ->
            if includeUnused then
                ( model, Random.generate Reorder (Random.List.shuffle model.card.values) )

            else
                let
                    amount =
                        Layout.amountOfSquares model.card.layout

                    ( used, unused ) =
                        Utils.split amount model.card.values
                in
                ( model
                , Random.generate Reorder
                    (Random.List.shuffle used
                        |> Random.map (\shuffled -> shuffled ++ unused)
                    )
                )

        Reorder values ->
            ( { model | card = Card.setValues values model.card }, Cmd.none )

        Save ->
            ( model, saveOut model.card.name )

        ClearErrors ->
            ( { model | errors = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        CodeMsg codeMsg ->
            case codeMsg of
                Code.Loaded card ->
                    ( { model | card = card }, Cmd.none )

                Code.Saved compressedCode ->
                    ( { model | code = Just compressedCode }, Navigation.pushUrl key (codeUrl compressedCode) )

                Code.Error message ->
                    ( { model | errors = message :: model.errors }, Cmd.none )

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
            , Cmd.none
            )


codeUrl : Code.CompressedCode -> String
codeUrl code =
    Url.Builder.custom Url.Builder.Relative [] [] (Just code)


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


errors : List Error -> Html Msg
errors messages =
    Html.div [ Attr.class "errors" ]
        [ Html.div [ Attr.class "heading" ]
            [ Html.h2 [] [ Html.text "Error" ]
            , Html.button
                [ Attr.class "pure-button pure-button-primary"
                , Html.onClick ClearErrors
                ]
                [ Icon.timesCircle ]
            ]
        , Html.p []
            [ Html.text "Whoops, there was an error. Please "
            , Html.a [ Attr.href "https://github.com/ABinaryGeek/bingo-card-gen/issues/new" ]
                [ Html.text "report this as a bug." ]
            ]
        , Html.ol [] (List.map error messages)
        ]


error : Error -> Html msg
error message =
    Html.li [ Attr.class "error" ] [ Html.text message ]


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


share : Maybe String -> Html Msg
share code =
    Html.div []
        [ Html.h2 [] [ Html.text "Share" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            (code
                |> Maybe.map
                    (\c ->
                        [ linkView c
                        , saveImage
                        ]
                    )
                |> Maybe.withDefault []
            )
        ]


linkEdit : String -> Html Msg
linkEdit code =
    linkView code


linkView : String -> Html Msg
linkView code =
    Html.div [ Attr.class "change-name pure-control-group" ]
        [ Html.label [ Attr.for "view-field" ] [ Html.text "Link To Edit: " ]
        , Html.div [ Attr.class "form-row" ]
            [ Html.input
                [ Attr.id "view-field"
                , Attr.class "pure-input-1"
                , Attr.value ("https://abinarygeek.github.io/bingo-card-gen/#" ++ code)
                , Attr.readonly True
                ]
                []
            , Html.button
                [ Attr.class "copy-button pure-button"
                , Attr.attribute "data-clipboard-target" "#view-field"
                ]
                [ Icon.copy ]
            ]
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
                , String.fromInt (Layout.amountOfSquares layout)
                , " Squares)"
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
