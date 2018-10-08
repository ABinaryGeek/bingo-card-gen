module Bingo.Editor.Model exposing
    ( Drag
    , DropTarget(..)
    , Editor
    , Error
    )

import Bingo.Card.Code as Code
import Bingo.Card.Model exposing (Card)
import Bingo.Model exposing (..)
import Html5.DragDrop as DragDrop


type DropTarget
    = ValueTarget Value
    | BinTarget


type alias Error = String


type alias Editor =
    { code : Maybe Code.CompressedCode
    , card : Card
    , newValueInput : String
    , dragDrop : DragDrop.Model Value DropTarget
    , errors : List Error
    }


type alias Drag =
    { draggedValue : Value
    , hoveredValue : Maybe DropTarget
    }
