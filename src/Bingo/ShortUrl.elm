module Bingo.ShortUrl exposing
    ( Msg(..)
    , ShortUrl(..)
    , request
    )

import Bingo.BaseUrl exposing (BaseUrl)
import Bingo.Page as Page
import Dict
import Http


type ShortUrl
    = Unknown
    | Requested
    | Known String


type Msg id
    = Response id (Result String String)


request : String -> BaseUrl -> Maybe Page.Reference -> id -> Cmd (Msg id)
request urlShortener baseUrl reference id =
    let
        url =
            Page.externalUrl baseUrl reference

        body =
            Http.multipartBody [ Http.stringPart "url" url ]
    in
    Http.post
        { url = urlShortener
        , body = body
        , expect = expectLocationHeader url (Response id)
        }


expectLocationHeader : String -> (Result String String -> msg) -> Http.Expect msg
expectLocationHeader url toMsg =
    Http.expectStringResponse toMsg (locationFromResponse url)


locationFromResponse : String -> Http.Response String -> Result String String
locationFromResponse url response =
    case response of
        Http.GoodStatus_ metadata body ->
            if body == url then
                case Dict.get "location" metadata.headers of
                    Just value ->
                        Ok value

                    Nothing ->
                        case Dict.get "Location" metadata.headers of
                            Just value ->
                                Ok value

                            Nothing ->
                                Err "Short URL service didn't return expected result."

            else
                Err "Short URL service returned a different URL."

        _ ->
            Err "Network error trying to get short URL. Try again later."
