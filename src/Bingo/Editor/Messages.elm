module Bingo.Editor.Messages exposing (Msg(..))

import Bingo.Card.Model exposing (Card)
import Bingo.Card.Code as Code
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
    | Save
    | ClearErrors
    | NoOp
    | CodeMsg Code.Msg
    | DragDropMsg (DragDrop.Msg Value DropTarget)
