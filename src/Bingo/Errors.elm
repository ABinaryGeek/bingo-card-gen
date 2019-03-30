module Bingo.Errors exposing
    ( Error
    , Errors
    , Msg(..)
    , add
    , addMany
    , init
    , maybeAdd
    , update
    , view
    )

import Bingo.Icon as Icon
import Bingo.Utils as Utils
import Html as Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type alias Error =
    String


type alias Errors =
    List Error


type Msg
    = ClearErrors


init : Errors
init =
    []


update : Msg -> Errors -> Errors
update msg _ =
    case msg of
        ClearErrors ->
            []


add : Error -> Errors -> Errors
add error model =
    error :: model


addMany : List Error -> Errors -> Errors
addMany errors model =
    errors ++ model


maybeAdd : Maybe Error -> Errors -> Errors
maybeAdd maybeError model =
    Utils.singletonOrEmpty maybeError ++ model


view : Errors -> List (Html Msg)
view model =
    if List.isEmpty model then
        []

    else
        [ Html.div [ HtmlA.class "errors" ]
            [ Html.div [ HtmlA.class "heading" ]
                [ Html.h2 [] [ Html.text "Error" ]
                , Html.button
                    [ HtmlA.class "pure-button pure-button-primary"
                    , HtmlE.onClick ClearErrors
                    ]
                    [ Icon.timesCircle ]
                ]
            , Html.p []
                [ Html.text "Whoops, there was an error. Please "
                , Html.a [ HtmlA.href "https://github.com/ABinaryGeek/bingo-card-gen/issues/new" ]
                    [ Html.text "report this as a bug." ]
                ]
            , Html.ol [] (List.map viewError model)
            ]
        ]



{- Private -}


viewError : Error -> Html msg
viewError message =
    Html.li [ HtmlA.class "error" ] [ Html.text message ]
