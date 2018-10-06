module Bingo.Editor.ValueList exposing (view)

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (Card)
import Bingo.Editor.Messages exposing (..)
import Bingo.Editor.Model exposing (..)
import Bingo.Model exposing (..)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Html
import Html5.DragDrop


view : (Value -> List (Html.Attribute Msg)) -> Card -> String -> Maybe Drag -> Html Msg
view attributes card newValueInput drag =
    let
        amount =
            Layout.amountOfSquares card.layout

        ( used, unused ) =
            Utils.split amount card.values

        bin =
            Maybe.map binView drag |> Maybe.withDefault []

        itemH =
            valueListItem attributes
    in
    Html.div [ Attr.class "container" ]
        ([ Html.h2 [] [ Html.text "Squares" ]
         , Html.p [] [ Html.text "Add values here. You can drag and drop them to swap squares around, or delete them." ]
         , controls card newValueInput
         , Html.hr [] []
         , Html.ul
            [ Attr.class "values" ]
            (List.concat
                [ List.map (itemH True) used
                , List.map (itemH False) unused
                ]
            )
         ]
            ++ bin
        )


controls : Card -> String -> Html Msg
controls card newValueInput =
    Html.form
        [ Attr.class "pure-form pure-form-stacked", Html.onSubmit NoOp ]
        (List.concat
            [ addValueControl newValueInput card
            , randomiseOrder card.values
            ]
        )


addValueControl : String -> Card -> List (Html Msg)
addValueControl input card =
    [ Html.div [ Attr.class "add-value pure-control-group" ]
        [ Html.label [ Attr.for "add-value-field" ] [ Html.text "Value:" ]
        , Html.input
            [ Attr.value input
            , Attr.id "add-value-field"
            , Html.onInput UpdateNewValueField
            ]
            []
        ]
    , Html.div [ Attr.class "pure-controls" ]
        [ Html.button
            [ Html.onClick AddNewValue
            , Attr.class "pure-button pure-button-primary"
            , Attr.disabled (not (Card.couldAdd input card))
            ]
            [ Html.text "Add"
            ]
        ]
    ]


randomiseOrder : List Value -> List (Html Msg)
randomiseOrder values =
    [ Html.div [ Attr.class "pure-controls" ]
        [ Html.button [ Html.onClick Randomise, Attr.class "pure-button" ]
            [ Html.text "Randomize Order"
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


binView : Drag -> List (Html Msg)
binView drag =
    [ Html.div
        ([ Attr.class "bin" ] ++ Html5.DragDrop.droppable DragDropMsg BinTarget)
        [ Html.text "Delete" ]
    ]
