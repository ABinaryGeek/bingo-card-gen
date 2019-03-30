module Bingo.Viewer.Messages exposing (Msg(..))


type Msg
    = Rotations (List Int)
    | ToggleStamp Int
    | Save
