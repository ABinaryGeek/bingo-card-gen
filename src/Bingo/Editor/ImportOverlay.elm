module Bingo.Editor.ImportOverlay exposing
    ( ImportOverlay
    , Msg(..)
    , Result(..)
    , init
    , update
    , view
    )

import Bingo.Icon as Icon
import Bingo.Model exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type Msg
    = Show
    | Update String
    | Done
    | Cancel


type Result
    = Import (List Value)
    | NoResult


type alias ImportOverlay =
    { input : Maybe String
    }


init : ImportOverlay
init =
    { input = Nothing }


update : Msg -> ImportOverlay -> ( ImportOverlay, Result )
update msg model =
    case msg of
        Show ->
            ( { model | input = Just "" }, NoResult )

        Update value ->
            ( { model | input = Just value }, NoResult )

        Done ->
            let
                result =
                    model.input |> Maybe.map (String.lines >> List.map String.trim >> Import)
            in
            ( { model | input = Nothing }, result |> Maybe.withDefault NoResult )

        Cancel ->
            ( { model | input = Nothing }, NoResult )


view : ImportOverlay -> List (Html Msg)
view model =
    case model.input of
        Just value ->
            [ Html.div [ HtmlA.class "import-overlay" ]
                [ Html.div [ HtmlA.class "import-content" ]
                    [ Html.textarea [ HtmlE.onInput Update ] [ Html.text value ]
                    , Html.div [ HtmlA.class "import-controls pure-control-group" ]
                        [ Html.button
                            [ HtmlA.class "pure-button"
                            , HtmlE.onClick Done
                            ]
                            [ Icon.fileImport, Html.text " Import" ]
                        , Html.button
                            [ HtmlA.class "pure-button"
                            , HtmlE.onClick Cancel
                            ]
                            [ Icon.timesCircle, Html.text " Cancel" ]
                        ]
                    ]
                ]
            ]

        Nothing ->
            []
