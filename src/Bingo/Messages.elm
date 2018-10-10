module Bingo.Messages exposing (Msg(..))

import Bingo.Card.Code as Code
import Bingo.Config as Config exposing (Config)
import Bingo.Editor.Messages as Editor
import Bingo.Errors as Errors
import Bingo.Page as Page
import Bingo.Viewer.Messages as Viewer
import Browser
import Http
import Url exposing (Url)


type Msg
    = NoOp
    | UrlChange Url
    | LinkFollowed Browser.UrlRequest
    | LoadConfig (Result Http.Error Config)
    | EditMsg Editor.Msg
    | ViewMsg Viewer.Msg
    | PageMsg Page.Msg
    | ErrorMsg Errors.Msg
