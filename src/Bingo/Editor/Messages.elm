module Bingo.Editor.Messages exposing (Msg(..))

import Bingo.Card.Model exposing (Card)
import Bingo.Editor.Model exposing (..)
import Bingo.Model exposing (..)
import Html5.DragDrop as DragDrop


type Msg
    = AddNewValue
    | AddGivenValue String
    | UpdateNewValueField String
    | Resize String
    | ChangeName String
    | ToggleFreeSquare
    | Randomise { includeUnused : Bool }
    | Reorder (List Value)
    | Load Card
    | UpdateCode String
    | Save
    | NoOp
    | DragDropMsg (DragDrop.Msg Value DropTarget)
