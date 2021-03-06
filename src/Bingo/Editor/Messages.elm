module Bingo.Editor.Messages exposing (ColorInputTarget(..), Msg(..), ShortUrlTarget(..))

import Bingo.Editor.ImportOverlay as ImportOverlay exposing (ImportOverlay)
import Bingo.Editor.Model exposing (..)
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
    | ColorChanged ColorInputTarget String


type ShortUrlTarget
    = ViewShortUrl
    | EditShortUrl


type ColorInputTarget
    = TitleColorInput
    | BackgroundColorInput
