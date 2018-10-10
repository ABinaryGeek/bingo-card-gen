module Bingo.Viewer.Messages exposing (Msg(..))

import Bingo.Card.Code as Code
import Bingo.Errors as Errors
import Bingo.Model exposing (Value)


type Msg
    = Rotations (List Int)
    | ToggleStamp Int
    | Save
