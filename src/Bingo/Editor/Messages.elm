module Bingo.Editor.Messages exposing (Msg(..), ShortUrlTarget(..))

import Bingo.Card.Code as Code
import Bingo.Card.Model exposing (Card)
import Bingo.Editor.ImportOverlay as ImportOverlay exposing (ImportOverlay)
import Bingo.Editor.Model exposing (..)
import Bingo.Errors as Errors
import Bingo.Model exposing (..)
import Bingo.ShortUrl as ShortUrl exposing (ShortUrl)
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
    | Shorten ShortUrlTarget
    | NoOp
    | ShortUrlMsg (ShortUrl.Msg ShortUrlTarget)
    | ImportOverlayMsg ImportOverlay.Msg
    | DragDropMsg (DragDrop.Msg Value DropTarget)


type ShortUrlTarget
    = ViewShortUrl
    | EditShortUrl
