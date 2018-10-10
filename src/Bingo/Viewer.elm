module Bingo.Viewer exposing
    ( load
    , update
    , view
    )

import Bingo.Card as Card
import Bingo.Card.Code as Code
import Bingo.Card.Layout as Layout
import Bingo.Card.Model as Card exposing (Card)
import Bingo.Card.TextBox as TextBox
import Bingo.Card.View as Card
import Bingo.Icon as Icon
import Bingo.Page as Page exposing (Page)
import Bingo.Save as Save
import Bingo.Utils as Utils
import Bingo.Viewer.Messages exposing (..)
import Bingo.Viewer.Model exposing (..)
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Random
import Svg exposing (Svg)
import Svg.Attributes as SvgA


load : Code.Out Msg -> TextBox.TextBoxesOut Msg -> Card.Stamped -> ( Viewer, Cmd Msg )
load codeOut textBoxesOut card =
    let
        squares =
            Layout.amountOfSquares card.card.layout
    in
    ( { stampedCard = card
      , rotations = List.repeat squares 0
      }
    , Cmd.batch
        [ generateRandomRotations squares
        , Card.onCardLoad textBoxesOut (Page.View card)
        ]
    )


view : Maybe Page.Reference -> Viewer -> Html Msg
view reference model =
    Html.div [ HtmlA.class "viewer" ]
        [ Html.div [ HtmlA.class "card-container" ]
            [ Card.viewWithOverlay (stamps model ++ lines model) clickHandler model.stampedCard.card ]
        , Html.a
            [ HtmlA.href (reference |> Maybe.map Page.referenceAsEdit |> Page.url)
            , HtmlA.target "_blank"
            , HtmlA.class "edit-link"
            , HtmlA.title "Edit this card."
            ]
            [ Icon.edit ]
        , Html.button
            [ HtmlA.class "save-link"
            , HtmlA.title "Save this card as a PNG."
            , HtmlE.onClick Save
            ]
            [ Icon.save ]
        ]


update : Save.Out Msg -> Code.Out Msg -> TextBox.TextBoxesOut Msg -> Navigation.Key -> Msg -> Viewer -> ( Viewer, Cmd Msg )
update saveOut codeOut textBoxesOut key msg model =
    let
        ( viewer, cmd ) =
            internalUpdate saveOut key msg model

        stampedCard =
            viewer.stampedCard

        commands =
            if stampedCard /= model.stampedCard then
                Cmd.batch [ cmd, Card.onCardChange codeOut textBoxesOut (Page.View stampedCard) ]

            else
                cmd
    in
    ( { viewer | stampedCard = stampedCard }, commands )



{- Private -}


internalUpdate : Save.Out Msg -> Navigation.Key -> Msg -> Viewer -> ( Viewer, Cmd Msg )
internalUpdate saveOut key msg model =
    case msg of
        Rotations rotations ->
            ( { model | rotations = rotations }, Cmd.none )

        ToggleStamp index ->
            let
                card =
                    model.stampedCard

                updatedCard =
                    { card | stamps = Stamps.toggle (Stamps.atIndex index) card.stamps }
            in
            ( { model | stampedCard = updatedCard }, Cmd.none )

        Save ->
            ( model, Save.save saveOut model.stampedCard.card.name )


clickHandler : Int -> Card.Square -> List (Svg.Attribute Msg)
clickHandler index value =
    [ HtmlE.onClick (ToggleStamp index) ]


lines : Viewer -> List (Svg msg)
lines model =
    let
        stampedCard =
            model.stampedCard

        layout =
            stampedCard.card.layout

        spacePerSquare =
            Layout.squareSpace Layout.gridSpace layout.size

        endPoints =
            Stamps.findLines layout stampedCard.stamps
                |> List.map (Stamps.ends layout)
    in
    [ Svg.g
        [ SvgA.class "lines-overlay overlay"
        , SvgA.transform ("translate (0 " ++ String.fromFloat Layout.headerSpace ++ ")")
        ]
        (endPoints |> List.map (line spacePerSquare))
    ]


line : Float -> ( ( Int, Int ), ( Int, Int ) ) -> Svg msg
line spacePerSquare ends =
    let
        ( start, end ) =
            ends

        ( x1, y1 ) =
            Layout.squarePos (Tuple.first start) (Tuple.second start) spacePerSquare

        ( x2, y2 ) =
            Layout.squarePos (Tuple.first end) (Tuple.second end) spacePerSquare

        halfSpace =
            spacePerSquare / 2
    in
    Svg.line
        [ SvgA.class "line"
        , SvgA.x1 (String.fromFloat (x1 + halfSpace))
        , SvgA.y1 (String.fromFloat (y1 + halfSpace))
        , SvgA.x2 (String.fromFloat (x2 + halfSpace))
        , SvgA.y2 (String.fromFloat (y2 + halfSpace))
        ]
        []


stamps : Viewer -> List (Svg msg)
stamps model =
    let
        layout =
            model.stampedCard.card.layout

        membership =
            Stamps.membership layout model.stampedCard.stamps

        membersWithRotations =
            List.map2 Tuple.pair model.rotations membership

        findRowMembers =
            Layout.rowMembers membersWithRotations layout.size

        spacePerSquare =
            Layout.squareSpace Layout.gridSpace layout.size

        scaleFactor =
            (spacePerSquare * 0.8) / 100.0
    in
    [ Svg.defs []
        [ Svg.path
            [ SvgA.id "star"
            , SvgA.d "M-14-15C0-62 0-62 14-15c46 0 46 0 9 28C37 59 37 59 0 31c-37 28-37 28-23-18-37-28-37-28 9-28"
            , SvgA.transform ("scale(" ++ String.fromFloat scaleFactor ++ ")")
            ]
            []
        ]
    , Svg.g [ SvgA.class "stamps overlay", SvgA.transform ("translate (0 " ++ String.fromFloat Layout.headerSpace ++ ")") ]
        (List.range 0 (layout.size - 1)
            |> List.map
                (\rowIndex ->
                    rowStamps spacePerSquare rowIndex (findRowMembers rowIndex)
                )
        )
    ]


rowStamps : Float -> Int -> List ( Rotation, Bool ) -> Svg msg
rowStamps spacePerSquare index members =
    let
        y =
            Layout.pos index spacePerSquare + (0.5 * spacePerSquare)
    in
    Svg.g [ SvgA.transform ("translate (0 " ++ String.fromFloat y ++ ")") ]
        (List.indexedMap (stamp y spacePerSquare) members |> List.concat)


stamp : Float -> Float -> Int -> ( Rotation, Bool ) -> List (Svg msg)
stamp y spacePerSquare column rotationExists =
    let
        ( rotation, exists ) =
            rotationExists

        x =
            Layout.pos column spacePerSquare + (0.5 * spacePerSquare)
    in
    if exists then
        [ Svg.use
            [ SvgA.xlinkHref "#star"
            , SvgA.class "stamp"
            , SvgA.transform
                ("translate("
                    ++ String.fromFloat x
                    ++ " 0) rotate("
                    ++ String.fromInt rotation
                    ++ ")"
                )
            ]
            []
        ]

    else
        []


generateRandomRotations : Int -> Cmd Msg
generateRandomRotations count =
    randomRotations count |> Random.generate Rotations


randomRotations : Int -> Random.Generator (List Rotation)
randomRotations count =
    Random.list count (Random.int 0 360)
