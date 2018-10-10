module Bingo.Messages exposing (Msg(..))

import Bingo.Card.Code as Code
import Bingo.Editor.Messages as Editor
import Bingo.Errors as Errors
import Bingo.Page as Page
import Bingo.Viewer.Messages as Viewer
import Browser
import Url exposing (Url)


type Msg
    = NoOp
    | UrlChange Url
    | LinkFollowed Browser.UrlRequest
    | EditMsg Editor.Msg
    | ViewMsg Viewer.Msg
    | PageMsg Page.Msg
    | ErrorMsg Errors.Msg
