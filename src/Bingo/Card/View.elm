module Bingo.Card.View exposing
    ( view
    , viewWithOverlay
    )

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Card.Square as Square
import Bingo.Editor.Messages
import Bingo.Model as Model exposing (Value)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Html
import Svg exposing (Svg)
import Svg.Attributes as SvgA


type alias AttributeInjector msg =
    Int -> Square -> List (Svg.Attribute msg)


view : Card -> Html msg
view card =
    viewWithOverlay [] (\i -> \s -> []) card


viewWithOverlay : List (Svg msg) -> AttributeInjector msg -> Card -> Html msg
viewWithOverlay overlay attributeInjector card =
    let
        width =
            Layout.gridSpace

        height =
            Layout.gridSpace + Layout.headerSpace

        size =
            card.layout.size

        spacePerSquare =
            Layout.squareSpace Layout.gridSpace size

        attrs =
            Square.squares card.layout card.values |> List.indexedMap attributeInjector
    in
    Svg.svg
        [ SvgA.class "bingo-card"
        , SvgA.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
        , SvgA.preserveAspectRatio "xMidYMid meet"
        ]
        ([ defs spacePerSquare size
         , Svg.rect
            ([ SvgA.class "background"
             ]
                ++ widthAndHeight width height
            )
            []
         , Svg.g
            [ SvgA.class "grid"
            , translate 0 Layout.headerSpace
            ]
            (grid card attrs width)
         , Svg.g [ SvgA.class "text-overlay overlay" ]
            [ Svg.g [ SvgA.class "titles" ] []
            , Svg.g [ SvgA.class "values" ] []
            ]
         ]
            ++ overlay
        )



{- Private -}


defs : Float -> Int -> Svg msg
defs spacePerSquare size =
    Svg.defs []
        [ squareDef spacePerSquare
        ]


squareDef : Float -> Svg msg
squareDef spacePerSquare =
    Svg.rect
        (List.concat
            [ [ SvgA.id "square", SvgA.class "square" ]
            , sizeAttrs spacePerSquare
            ]
        )
        []


grid : Card -> List (List (Svg.Attribute msg)) -> Float -> List (Svg msg)
grid card attrs space =
    let
        size =
            card.layout.size

        spacePerSquare =
            Layout.squareSpace space size

        findRowAttrs =
            Layout.rowMembers attrs size
    in
    List.range 0 (size - 1) |> List.map (\index -> squareRow spacePerSquare (findRowAttrs index) size index)


squareRow : Float -> List (List (Svg.Attribute msg)) -> Int -> Int -> Svg msg
squareRow spacePerSquare attrs columns row =
    let
        squares =
            List.map2 (squareRef spacePerSquare) attrs (List.range 0 (columns - 1))
    in
    squares |> Svg.g [ SvgA.class "row", translate 0 (Layout.pos row spacePerSquare) ]


squareRef : Float -> List (Svg.Attribute msg) -> Int -> Svg msg
squareRef spacePerSquare attrs column =
    Svg.use ([ SvgA.xlinkHref "#square", translate (Layout.pos column spacePerSquare) 0 ] ++ attrs) []


translate : Float -> Float -> Svg.Attribute msg
translate x y =
    SvgA.transform
        ("translate("
            ++ String.fromFloat x
            ++ " "
            ++ String.fromFloat y
            ++ ")"
        )


sizeAttrs : Float -> List (Svg.Attribute msg)
sizeAttrs size =
    widthAndHeight size size


widthAndHeight : Float -> Float -> List (Svg.Attribute msg)
widthAndHeight width height =
    [ SvgA.width (String.fromFloat width)
    , SvgA.height (String.fromFloat height)
    ]
