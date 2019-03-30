module Bingo.Messages exposing (Back(..), Msg(..))

import Bingo.Config exposing (Config)
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


type Back
    = Error Errors.Error
    | NoBackMessage
