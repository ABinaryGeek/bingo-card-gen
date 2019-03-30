module Bingo.ShortUrl exposing (Msg(..), ShortUrl(..), request)

import Bingo.BaseUrl exposing (BaseUrl)
import Bingo.Page as Page
import Dict
import Http
import Task


type ShortUrl
    = Unknown
    | Requested
    | Known String


type Msg id
    = Response id (Result Http.Error String)


request : String -> BaseUrl -> Maybe Page.Reference -> id -> Cmd (Msg id)
request urlShortener baseUrl reference id =
    let
        url =
            Page.externalUrl baseUrl reference

        body =
            Http.multipartBody [ Http.stringPart "url" url ]
    in
    Http.request
        { method = "post"
        , headers = []
        , url = urlShortener
        , body = body
        , expect = Http.expectStringResponse expectedResponse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (Response id)


expectedResponse : Http.Response String -> Result String String
expectedResponse response =
    case response.status.code of
        201 ->
            case Dict.get "Location" response.headers of
                Just value ->
                    Ok value

                Nothing ->
                    Err "No location header."

        _ ->
            Err "Incorrect response."


{-| Swap this in for request to test without actually hitting the server.
-}
testRequest : String -> BaseUrl -> Maybe Page.Reference -> id -> Cmd (Msg id)
testRequest urlShortener baseUrl reference id =
    Task.succeed () |> Task.perform (\_ -> Response id (Ok "shorturl"))
