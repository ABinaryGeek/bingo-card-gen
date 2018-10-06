module Bingo.Editor exposing
    ( init
    , update
    , view
    )

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Card.Save as Card
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Editor.ValueList as ValueList
import Bingo.Model exposing (Value)
import Bingo.Utils as Utils
import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Html
import Html5.DragDrop as DragDrop
import Random
import Random.List
import Task


init : Card -> Editor
init card =
    { card = card
    , newValueInput = ""
    , dragDrop = DragDrop.init
    }


update : Msg -> Editor -> ( Editor, Cmd Msg )
update msg model =
    case msg of
        AddNewValue ->
            ( { model
                | card = Card.add model.newValueInput model.card
                , newValueInput = ""
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "add-value-field")
            )

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

        Randomise ->
            ( model, Random.generate Reorder (Random.List.shuffle model.card.values) )

        Reorder values ->
            ( { model | card = Card.setValues values model.card }, Cmd.none )

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

        NoOp ->
            ( model, Cmd.none )


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
    in
    Html.div [ Attr.class "editor" ]
        [ controls model.card
        , ValueList.view attributes model.card model.newValueInput (drag model.dragDrop)
        , Html.div [ Attr.class "container" ] [ Card.view attributes model.card ]
        ]


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


controls : Card -> Html Msg
controls card =
    Html.div [ Attr.class "container" ]
        [ Html.h2 [] [ Html.text "Settings" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ changeNameControl card.name
            , changeSizeControl card.layout
            , freeSquareControl card.layout
            ]
        , Html.hr [] []
        , Html.h2 [] [ Html.text "Share" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ linkView card
            , linkEdit card
            ]
        ]


linkEdit : Card -> Html Msg
linkEdit card =
    linkView card


linkView : Card -> Html Msg
linkView card =
    Html.div [ Attr.class "change-name pure-control-group" ]
        [ Html.label [ Attr.for "view-field" ] [ Html.text "View Link: " ]
        , Html.input
            [ Attr.id "view-field"
            , Attr.value ("http://localhost:8000/#?view=" ++ Card.save card)
            , Attr.readonly True
            ]
            []
        ]


changeNameControl : String -> Html Msg
changeNameControl name =
    Html.div [ Attr.class "change-name pure-control-group" ]
        [ Html.label [ Attr.for "name-field" ] [ Html.text "Name: " ]
        , Html.input
            [ Attr.id "name-field"
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
            , Html.onClick ToggleFreeSquare
            , Attr.checked (Layout.freeSquareValid layout && layout.free)
            , Attr.disabled (not (Layout.freeSquareValid layout))
            ]
            []
        ]


highlightedClass : Value -> Maybe Value -> List (Html.Attribute msg)
highlightedClass value target =
    if Just value == target then
        [ Attr.class "highlighted" ]

    else
        []


dragDropTarget : Value -> List (Html.Attribute Msg)
dragDropTarget value =
    DragDrop.draggable DragDropMsg value ++ DragDrop.droppable DragDropMsg (ValueTarget value)
