module Bingo.Editor.Model exposing
    ( Drag
    , DropTarget(..)
    , Editor
    )

import Bingo.Card.Model exposing (Card)
import Bingo.Editor.ImportOverlay as ImportOverlay exposing (ImportOverlay)
import Bingo.Model exposing (..)
import Html5.DragDrop as DragDrop


type DropTarget
    = ValueTarget Value
    | BinTarget


type alias Editor =
    { card : Card
    , newValueInput : String
    , importOverlay : ImportOverlay
    , dragDrop : DragDrop.Model Value DropTarget
    }


type alias Drag =
    { draggedValue : Value
    , hoveredValue : Maybe DropTarget
    }
