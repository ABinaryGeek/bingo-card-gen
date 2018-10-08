module Bingo.Card.View exposing (view)

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Editor.Messages
import Bingo.Model as Model exposing (Value)
import Bingo.Utils as Utils
import Html exposing (Html)
import Html.Attributes as Html
import Svg exposing (Svg)
import Svg.Attributes as SvgA


view : Card -> Html msg
view card =
    let
        width =
            Layout.gridSpace

        height =
            Layout.gridSpace + Layout.headerSpace
    in
    Svg.svg
        [ SvgA.class "bingo-card"
        , SvgA.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
        , SvgA.preserveAspectRatio "xMidYMid meet"
        ]
        [ Svg.rect
            [ SvgA.class "background"
            , SvgA.width (String.fromFloat width)
            , SvgA.height (String.fromFloat height)
            ]
            []
        , Svg.g
            [ SvgA.class "grid"
            , translate 0 Layout.headerSpace
            ]
            (grid card Layout.gridSpace)
        , Svg.g [ SvgA.class "text-overlay" ]
            [ Svg.g [ SvgA.class "titles" ] []
            , Svg.g [ SvgA.class "values" ] []
            ]
        ]


css : String -> Svg msg
css url =
    Svg.node "link"
        [ Html.attribute "xmlns" "http://www.w3.org/1999/xhtml"
        , Html.attribute "href" url
        , Html.rel "stylesheet"
        , SvgA.type_ "text/css"
        ]
        []


grid : Card -> Float -> List (Svg msg)
grid card space =
    [ squareRows space card.layout
    ]


squareRows : Float -> Layout -> Svg msg
squareRows space layout =
    let
        size =
            layout.size

        spacePerSquare =
            Layout.squareSpace space size

        rows =
            List.range 0 (size - 1)
                |> List.map
                    (\index ->
                        squareRow spacePerSquare index size
                    )
    in
    rows |> Svg.g [ SvgA.class "rows" ]


squareRow : Float -> Int -> Int -> Svg msg
squareRow space row columns =
    let
        squares =
            List.range 0 (columns - 1)
                |> List.map
                    (\column -> square column row space)
    in
    squares |> Svg.g [ SvgA.class "row" ]


square : Int -> Int -> Float -> Svg msg
square column row space =
    let
        ( x, y ) =
            Layout.squarePos column row space
    in
    Svg.g
        [ SvgA.class "square"
        , translate x y
        ]
        [ Svg.rect
            (List.concat
                [ [ SvgA.class "background" ]
                , widthAndHeight space
                ]
            )
            []
        ]


translate : Float -> Float -> Svg.Attribute msg
translate x y =
    SvgA.transform
        ("translate("
            ++ String.fromFloat x
            ++ " "
            ++ String.fromFloat y
            ++ ")"
        )


widthAndHeight : Float -> List (Svg.Attribute msg)
widthAndHeight size =
    [ SvgA.width (String.fromFloat size)
    , SvgA.height (String.fromFloat size)
    ]
