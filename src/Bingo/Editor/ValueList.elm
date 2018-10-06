module Bingo.Editor.ValueList exposing
    ( view
    , viewControls
    )

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Icon as Icon
import Bingo.Model exposing (..)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Html
import Html5.DragDrop


commonValues : List String
commonValues =
    [ "Singing"
    , "Crying"
    , "Singing while crying"
    , "A phone is destroyed"
    , "A drill is invoked"
    , "Ears/noses are wiggled"
    , "Stevonnie"
    , "Non-Stevonnie fusion"
    , "Wolf is a good boy"
    , "Foreheads touched"
    , "Lapis uses a new nickname for Connie"
    ]


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


viewControls : Card -> String -> Html Msg
viewControls card newValueInput =
    Html.div [ Attr.class "container" ]
        [ Html.h2 [] [ Html.text "Values" ]
        , controls card newValueInput
        , Html.hr [] []
        , Html.h2 [] [ Html.text "Common" ]
        , Html.ul
            [ Attr.class "pure-form" ]
            [ Html.div [ Attr.class "add-value pure-control-group" ]
                (List.map (addCommonButton card) commonValues)
            ]
        ]


view : (Value -> List (Html.Attribute Msg)) -> Card -> Maybe Drag -> Html Msg
view attributes card drag =
    let
        amount =
            Layout.amountOfSquares card.layout

        ( used, unused ) =
            Utils.split amount card.values

        bin =
            Maybe.map binView drag |> Maybe.withDefault (Html.span [] [])

        itemH =
            valueListItem attributes
    in
    Html.div [ Attr.class "container" ]
        [ Html.h2 [] [ Html.text "Order" ]
        , Html.p [] [ Html.text "You can drag and drop values to swap squares around, or delete them." ]
        , Html.form
            [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
            [ randomiseOrder card.values
            ]
        , Html.ul
            [ Attr.class "values" ]
            (List.concat
                [ List.map (itemH True) used
                , List.map (itemH False) unused
                ]
            )
        , bin
        ]


controls : Card -> String -> Html Msg
controls card newValueInput =
    Html.form
        [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
        (List.concat
            [ addValueControl newValueInput card
            ]
        )


addValueControl : String -> Card -> List (Html Msg)
addValueControl input card =
    [ Html.div [ Attr.class "add-value pure-control-group" ]
        [ Html.label [ Attr.for "add-value-field" ] [ Html.text "Value:" ]
        , Html.div [ Attr.class "form-row" ]
            [ Html.input
                [ Attr.value input
                , Attr.id "add-value-field"
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
    ]


randomiseOrder : List Value -> Html Msg
randomiseOrder values =
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


valueListItem : (Value -> List (Html.Attribute msg)) -> Bool -> Value -> Html msg
valueListItem attributes used value =
    Html.li []
        [ Html.span
            ([ Attr.class "value", usedClass used ] ++ attributes value)
            [ Html.text value ]
        ]


usedClass : Bool -> Html.Attribute msg
usedClass used =
    Attr.class
        (case used of
            True ->
                "used"

            False ->
                "unused"
        )


binView : Drag -> Html Msg
binView drag =
    Html.div
        ([ Attr.class "bin" ] ++ Html5.DragDrop.droppable DragDropMsg BinTarget)
        [ Html.text "Delete" ]
