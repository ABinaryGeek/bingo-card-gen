module Bingo.Messages exposing (Msg(..))

import Bingo.Editor.Messages as Editor
import Bingo.Viewer.Messages as Viewer
import Html5.DragDrop as DragDrop


type Msg
    = ViewerMsg Viewer.Msg
    | EditorMsg Editor.Msg
