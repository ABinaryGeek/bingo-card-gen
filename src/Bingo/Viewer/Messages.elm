module Bingo.Viewer.Messages exposing (Msg(..))

import Bingo.ShortUrl as ShortUrl


type Msg
    = Rotations (List Int)
    | ToggleStamp Int
    | Save
    | ShortenURL
    | ShortUrlMsg (ShortUrl.Msg ())
