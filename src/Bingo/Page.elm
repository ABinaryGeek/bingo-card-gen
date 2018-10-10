module Bingo.Page exposing
    ( Msg(..)
    , Page(..)
    , Reference
    , card
    , externalUrl
    , load
    , pageAsEdit
    , pageAsView
    , processFlags
    , referenceAsEdit
    , referenceAsView
    , referenceFromFragment
    , referenceFromUrl
    , save
    , subscriptions
    , url
    )

import Bingo.Card.Code as Code
import Bingo.Card.Model as Card exposing (Card)
import Bingo.Utils as Utils
import Bingo.Viewer.Stamps as Stamps exposing (Stamps)
import Json.Decode as Json
import Json.Encode
import Url exposing (Url)
import Url.Builder


{-| Messages as a result of save/load commands.
-}
type Msg
    = Loaded Page
    | Saved Reference
    | Error String


{-| A reference to a particular card and view. An encoded version of a PageRequest.
-}
type Reference
    = EditR Code.Compressed
    | ViewR Stamps Code.Compressed


{-| A request for a page (a decoded reference).
-}
type Page
    = Edit Card
    | View Card.Stamped


{-| This subscription allows you to listen for responses from the port with
the result of your requests.
-}
subscriptions : Code.In -> Sub Msg
subscriptions inPort =
    Code.subscriptions inPort |> Sub.map inboundCode


{-| Save a card to a compressed code. Takes the port for doing this as the
first argument. Will result in a Saved or Error message.
-}
save : Code.Out msg -> Page -> Cmd msg
save outPort page =
    Code.encode outPort (card page) (pageSideCar page)


{-| Load a card from a reference. Takes the port for doing this as the
first argument. Will result in a Loaded or Error message.
-}
load : Code.Out msg -> Reference -> Cmd msg
load outPort reference =
    Code.decode outPort (code reference) (referenceSideCar reference)


{-| Generate a relative URL for a reference.
-}
url : Maybe Reference -> String
url reference =
    urlBuilder Url.Builder.Relative reference


{-| Generate an absolute URL for a reference.
-}
externalUrl : String -> Maybe Reference -> String
externalUrl origin reference =
    urlBuilder (Url.Builder.CrossOrigin origin) reference


{-| Generate a reference from a URL.
-}
referenceFromUrl : Url -> Maybe (Result String Reference)
referenceFromUrl givenUrl =
    givenUrl.fragment |> Maybe.map referenceFromFragment


{-| Generate a reference from the fragment of a URL.
-}
referenceFromFragment : String -> Result String Reference
referenceFromFragment fragmentString =
    let
        ( stampsString, givenCode ) =
            Utils.partition "#" fragmentString

        stampsResult =
            stampsString |> Maybe.map (Json.decodeString Stamps.decoder)
    in
    case stampsResult of
        Just stamps ->
            stamps |> Result.map (\s -> ViewR s givenCode) |> Result.mapError Json.errorToString

        Nothing ->
            Ok (EditR givenCode)


{-| Get the card for a page.
-}
card : Page -> Card
card page =
    case page of
        Edit unstampedCard ->
            unstampedCard

        View stampedCard ->
            stampedCard.card


{-| Process some data as though it were a subscription, useful for flags.
-}
processFlags : Json.Value -> Msg
processFlags value =
    value |> Code.processFlags |> inboundCode


{-| Get the edit page for the card from the given page.
-}
pageAsEdit : Page -> Page
pageAsEdit page =
    case page of
        Edit _ ->
            page

        View stampedCard ->
            Edit stampedCard.card


{-| Get the view page for the card from the given page.
-}
pageAsView : Page -> Page
pageAsView page =
    case page of
        Edit unstampedCard ->
            View { card = unstampedCard, stamps = Stamps.empty }

        View _ ->
            page


{-| Get the edit reference for the card from the given page reference.
-}
referenceAsEdit : Reference -> Reference
referenceAsEdit reference =
    case reference of
        EditR _ ->
            reference

        ViewR _ c ->
            EditR c


{-| Get the view reference for the card from the given page reference.
-}
referenceAsView : Reference -> Reference
referenceAsView reference =
    case reference of
        EditR c ->
            ViewR Stamps.empty c

        ViewR _ _ ->
            reference



{- Private -}


urlBuilder : Url.Builder.Root -> Maybe Reference -> String
urlBuilder root reference =
    Url.Builder.custom root [] [] (Maybe.map fragment reference)


fragment : Reference -> String
fragment reference =
    case reference of
        EditR c ->
            c

        ViewR stamps c ->
            (Stamps.encode stamps |> Json.Encode.encode 0) ++ "#" ++ c


inboundCode : Code.Msg -> Msg
inboundCode msg =
    case msg of
        Code.Encoded encodedCode sideCar ->
            case sideCar of
                Just encodedStamps ->
                    stampsOrError encodedStamps (\stamps -> Saved (ViewR stamps encodedCode))

                Nothing ->
                    Saved (EditR encodedCode)

        Code.Decoded decodedCard sideCar ->
            case sideCar of
                Just encodedStamps ->
                    stampsOrError encodedStamps
                        (\stamps ->
                            Loaded (View { card = decodedCard, stamps = stamps })
                        )

                Nothing ->
                    Loaded (Edit decodedCard)

        Code.Error error ->
            Error error


stampsOrError : Json.Value -> (Stamps -> Msg) -> Msg
stampsOrError encodedStamps map =
    case Json.decodeValue Stamps.decoder encodedStamps |> Result.mapError Json.errorToString of
        Ok stamps ->
            map stamps

        Err error ->
            Error error


pageSideCar : Page -> Maybe Json.Value
pageSideCar page =
    case page of
        Edit _ ->
            Nothing

        View stamped ->
            Just (Stamps.encode stamped.stamps)


code : Reference -> Code.Compressed
code reference =
    case reference of
        EditR c ->
            c

        ViewR _ c ->
            c


referenceSideCar : Reference -> Maybe Json.Value
referenceSideCar reference =
    case reference of
        EditR _ ->
            Nothing

        ViewR stamps _ ->
            Just (Stamps.encode stamps)
