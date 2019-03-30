module Bingo.Editor.ValueList exposing
    ( add
    , commonlyAdded
    , view
    )

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Editor.ImportOverlay as ImportOverlay exposing (ImportOverlay)
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Icon as Icon
import Bingo.Model exposing (..)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Html
import Html.Keyed as HtmlK
import Html5.DragDrop


addCommonButton : Card -> String -> Html Msg
addCommonButton card value =
    Html.li [ Attr.class "common-values" ]
        [ Html.button
            [ Html.onClick (AddGivenValue value)
            , Attr.class "pure-button"
            , Attr.disabled (not (Card.couldAdd value card))
            ]
            [ Icon.plus
            ]
        , Html.text " \""
        , Html.text value
        , Html.text "\""
        ]


add : Card -> String -> Html Msg
add card newValueInput =
    Html.div []
        [ Html.h2 [] [ Html.text "Add" ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ addValueControl newValueInput card
            , importValuesControl
            ]
        ]


addValueControl : String -> Card -> Html Msg
addValueControl input card =
    Html.div [ Attr.class "pure-control-group" ]
        [ Html.label [ Attr.for "add-value-field" ] [ Html.text "Square Text:" ]
        , Html.div [ Attr.class "form-row" ]
            [ Html.input
                [ Attr.value input
                , Attr.id "add-value-field"
                , Attr.class "pure-input-1"
                , Html.onInput UpdateNewValueField
                ]
                []
            , Html.button
                [ Html.onClick AddNewValue
                , Attr.class "pure-button pure-button-primary"
                , Attr.disabled (not (Card.couldAdd input card))
                ]
                [ Icon.plus
                , Html.text " Add"
                ]
            ]
        ]


importValuesControl : Html Msg
importValuesControl =
    Html.div [ Attr.class "pure-control-group" ]
        [ Html.label [] [ Html.text "Import:" ]
        , Html.button
            [ Attr.class "pure-button"
            , Html.onClick (ImportOverlayMsg ImportOverlay.Show)
            ]
            [ Icon.listUl, Html.text " Add Many" ]
        ]


commonlyAdded : List Value -> Card -> List (Html Msg)
commonlyAdded commonValues card =
    if List.isEmpty commonValues then
        []

    else
        [ Html.div [ Attr.class "container" ]
            [ Html.h2 [] [ Html.text "Commonly Added" ]
            , Html.ul
                [ Attr.class "pure-form" ]
                [ Html.div [ Attr.class "add-value pure-control-group" ]
                    (List.map (addCommonButton card) commonValues)
                ]
            ]
        ]


view : (Value -> List (Html.Attribute Msg)) -> Card -> Maybe Drag -> Html Msg
view attributes card drag =
    let
        valueList =
            if List.length card.values == 0 then
                Html.p [ Attr.class "no-values" ]
                    [ Icon.questionCircle
                    , Html.text " No values yet, try adding some."
                    ]

            else
                let
                    amount =
                        Layout.amountOfValues card.layout

                    itemH =
                        valueListItem attributes drag

                    ( used, unused ) =
                        Utils.split amount card.values
                in
                HtmlK.ul
                    [ Attr.class "values" ]
                    (List.concat
                        [ List.map (itemH True) used
                        , List.map (itemH False) unused
                        ]
                    )
    in
    Html.div [ Attr.class "container" ]
        [ Html.h2 [] [ Html.text "Order" ]
        , Html.p [] [ Html.text "You can drag a value to another to swap squares or to the bottom to delete them." ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ randomiseOrder
            ]
        , valueList
        , binView drag
        ]


randomiseOrder : Html Msg
randomiseOrder =
    Html.div [ Attr.class "pure-controls" ]
        [ Html.label [ Attr.for "add-value-field" ] [ Html.text "Shuffle:" ]
        , Html.div [ Attr.class "form-row" ]
            [ Html.button
                [ Html.onClick (Randomise { includeUnused = True })
                , Attr.class "pure-button"
                , Attr.title "Shuffle all values, including those that don't fit in the card."
                ]
                [ Icon.random
                , Html.text " All"
                ]
            , Html.button
                [ Html.onClick (Randomise { includeUnused = False })
                , Attr.class "pure-button"
                , Attr.title "Shuffle visible values, ignoring any that don't fit in the card."
                ]
                [ Icon.random
                , Html.text " Included"
                ]
            ]
        ]


valueListItem : (Value -> List (Html.Attribute msg)) -> Maybe Drag -> Bool -> Value -> ( String, Html msg )
valueListItem attributes drag used value =
    let
        isDragged =
            drag |> Maybe.map (\d -> d.draggedValue == value) |> Maybe.withDefault False
    in
    ( value
    , Html.li []
        [ Html.span
            (Attr.classList
                [ ( "value", True )
                , ( "unused", not used )
                , ( "dragged", isDragged )
                ]
                :: attributes value
            )
            [ Html.text value ]
        ]
    )


binView : Maybe Drag -> Html Msg
binView drag =
    Html.div
        ([ Attr.classList [ ( "bin", True ), ( "enabled", drag /= Nothing ) ]
         ]
            ++ Html5.DragDrop.droppable DragDropMsg BinTarget
        )
        [ Icon.trash ]
