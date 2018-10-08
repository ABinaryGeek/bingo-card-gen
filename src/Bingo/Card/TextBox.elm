module Bingo.Card.TextBox exposing (TextBoxesOut, render)

import Bingo.Card as Card
import Bingo.Card.Layout as Layout exposing (Layout)
import Bingo.Card.Model exposing (..)
import Bingo.Model exposing (Value)
import Json.Encode as Json


{-| The type of the outbound port required.
-}
type alias TextBoxesOut msg =
    Json.Value -> Cmd msg


{-| Send a message to the prot to render the appropriate text boxes for the
card.
-}
render : TextBoxesOut msg -> Card -> Cmd msg
render textBoxesOut card =
    encode card |> textBoxesOut



{- Private -}


encode : Card -> Json.Value
encode card =
    { title = titleTextBox card
    , squares = textBoxes card
    }
        |> encodeCardTextBoxes


type alias TextBox =
    { text : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias CardTextBoxes =
    { title : TextBox
    , squares : List TextBox
    }


encodeTextBox : TextBox -> Json.Value
encodeTextBox textBox =
    Json.object
        [ ( "text", Json.string <| textBox.text )
        , ( "x", Json.float <| textBox.x )
        , ( "y", Json.float <| textBox.y )
        , ( "width", Json.float <| textBox.width )
        , ( "height", Json.float <| textBox.height )
        ]


encodeCardTextBoxes : CardTextBoxes -> Json.Value
encodeCardTextBoxes cardTextBoxes =
    Json.object
        [ ( "title", encodeTextBox <| cardTextBoxes.title )
        , ( "squares", Json.list encodeTextBox <| cardTextBoxes.squares )
        ]


titleTextBox : Card -> TextBox
titleTextBox card =
    { text = card.name
    , x = Layout.padding
    , y = Layout.padding
    , width = Layout.gridSpace - Layout.padding * 2
    , height = Layout.headerSpace - Layout.padding
    }


textBoxes : Card -> List TextBox
textBoxes card =
    let
        size =
            card.layout.size

        squareValues =
            Card.squares card.layout card.values |> List.map Card.squareText
    in
    squareRows Layout.gridSpace size squareValues


squareRows : Float -> Int -> List Value -> List TextBox
squareRows space size values =
    let
        spacePerSquare =
            Layout.squareSpace space size

        findRowValues =
            rowValues values size
    in
    List.range 0 (size - 1)
        |> List.concatMap
            (\index ->
                squareRow spacePerSquare index (findRowValues index)
            )


rowValues : List Value -> Int -> Int -> List Value
rowValues values size row =
    values |> List.drop (row * size) |> List.take size


squareRow : Float -> Int -> List Value -> List TextBox
squareRow space row values =
    List.indexedMap
        (\column -> \value -> square column row space value)
        values


square : Int -> Int -> Float -> String -> TextBox
square column row space value =
    let
        ( x, y ) =
            Layout.squarePos column row space
    in
    { text = value
    , x = x + Layout.padding
    , y = y + Layout.padding + Layout.headerSpace
    , width = space - Layout.padding * 2
    , height = space - Layout.padding * 2
    }
