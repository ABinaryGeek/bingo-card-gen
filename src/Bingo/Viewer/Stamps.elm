module Bingo.Viewer.Stamps exposing
    ( Direction(..)
    , Line(..)
    , Stamp
    , Stamps
    , add
    , atIndex
    , decodeString
    , decoder
    , empty
    , encode
    , encodeString
    , ends
    , findLines
    , isSet
    , mapMembership
    , membership
    , remove
    , toggle
    )

import Bingo.Card.Layout as Card
import Bitwise
import Json.Decode
import Json.Encode
import Set exposing (Set)


{-| A set of stamps.
-}
type alias Stamps =
    Set Int


{-| A stamp at a given index.
-}
type alias Stamp =
    Int


{-| A line through the grid.
-}
type Line
    = Vertical Int
    | Horizontal Int
    | Diagonal Direction


{-| A direction for a diagonal line.
-}
type Direction
    = TopLeftBottomRight
    | BottomLeftTopRight


{-| Get a stamp representing a given index.
-}
atIndex : Int -> Stamp
atIndex index =
    index


{-| An empty stamp set.
-}
empty : Stamps
empty =
    Set.empty


{-| Add the given stamp to the set.
-}
add : Stamp -> Stamps -> Stamps
add stamp stamps =
    Set.insert stamp stamps


{-| Remove the given stamp from the set.
-}
remove : Stamp -> Stamps -> Stamps
remove stamp stamps =
    Set.remove stamp stamps


{-| Returns true if the stamp is in the set.
-}
isSet : Stamp -> Stamps -> Bool
isSet stamp stamps =
    Set.member stamp stamps


{-| Adds the stamp if it isn't in the set, removes it if it is.
-}
toggle : Stamp -> Stamps -> Stamps
toggle stamp stamps =
    if isSet stamp stamps then
        remove stamp stamps

    else
        add stamp stamps


{-| Applies the function to each possible stamp in order, giving if it is in the set.
-}
mapMembership : (Bool -> a) -> Card.Layout -> Stamps -> List a
mapMembership f layout stamps =
    List.range 0 (layout.size * layout.size - 1) |> List.map (atIndex >> (\stamp -> isSet stamp stamps) >> f)


{-| A list of each possible stamp in order, giving if it is in the set.
-}
membership : Card.Layout -> Stamps -> List Bool
membership layout stamps =
    mapMembership identity layout stamps


{-| Encode a stamps set to JSON.
-}
encode : Stamps -> Json.Encode.Value
encode stamps =
    stamps |> encodeString |> Json.Encode.string


encodeString : Stamps -> String
encodeString stamps =
    let
        highest =
            Set.foldr max 0 stamps

        amount =
            highest // intSize

        withLocations =
            Set.map asBit stamps
    in
    List.range 0 amount
        |> List.map
            (\i ->
                withLocations
                    |> Set.filter (hasIndex i)
                    |> Set.map Tuple.second
                    |> Set.foldl Bitwise.or 0
                    |> String.fromInt
                    |> (\s ->
                            if i == amount then
                                s

                            else
                                String.padLeft encodedIntLength '0' s
                       )
            )
        |> List.reverse
        |> String.concat


{-| Decoder for a stamps set from JSON.
-}
decoder : Json.Decode.Decoder Stamps
decoder =
    Json.Decode.string |> Json.Decode.map decodeString


decodeString : String -> Stamps
decodeString string =
    let
        partLength =
            encodedIntLength

        size =
            String.length string // partLength

        parts =
            List.range 0 size
                |> List.reverse
                |> List.map
                    (\i ->
                        string
                            |> String.dropRight (i * partLength)
                            |> String.right partLength
                            |> String.toInt
                            |> toSet i
                    )
    in
    List.foldl Set.union Set.empty parts


{-| Find lines of stamps crossing the grid.
-}
findLines : Card.Layout -> Stamps -> List Line
findLines layout stamps =
    let
        size =
            layout.size

        sizeRange =
            List.range 0 (size - 1)

        potentialLines =
            List.concat
                [ sizeRange |> List.map Vertical
                , sizeRange |> List.map Horizontal
                , [ Diagonal TopLeftBottomRight, Diagonal BottomLeftTopRight ]
                ]
    in
    potentialLines |> List.filter (lineStamped sizeRange size stamps)


{-| The coordinates of the ends of the given line.
-}
ends : Card.Layout -> Line -> ( ( Int, Int ), ( Int, Int ) )
ends layout line =
    let
        size =
            layout.size
    in
    lineToCoordinates [ 0, size - 1 ] size line |> tupleEnds



{- Private -}


tupleEnds : List ( Int, Int ) -> ( ( Int, Int ), ( Int, Int ) )
tupleEnds list =
    case list of
        first :: second :: [] ->
            ( first, second )

        _ ->
            ( ( 0, 0 ), ( 0, 0 ) )


lineStamped : List Int -> Int -> Stamps -> Line -> Bool
lineStamped sizeRange size stamps line =
    lineToIndexes sizeRange size line
        |> List.all (\stamp -> isSet (atIndex stamp) stamps)


lineToIndexes : List Int -> Int -> Line -> List Int
lineToIndexes sizeRange size line =
    lineToCoordinates sizeRange size line
        |> List.map (coordinateToIndex size)


lineToCoordinates : List Int -> Int -> Line -> List ( Int, Int )
lineToCoordinates sizeRange size line =
    let
        f =
            case line of
                Vertical column ->
                    \row -> ( column, row )

                Horizontal row ->
                    \column -> ( column, row )

                Diagonal TopLeftBottomRight ->
                    \index -> ( index, index )

                Diagonal BottomLeftTopRight ->
                    \index -> ( index, size - 1 - index )
    in
    List.map f sizeRange


coordinateToIndex : Int -> ( Int, Int ) -> Int
coordinateToIndex size pos =
    case pos of
        ( column, row ) ->
            column + row * size


intSize : Int
intSize =
    30


encodedIntLength : Int
encodedIntLength =
    Bitwise.shiftLeftBy intSize 1 |> String.fromInt |> String.length


asBit : Int -> ( Int, Int )
asBit index =
    ( index // intSize, Bitwise.shiftLeftBy (remainderBy intSize index) 1 )


hasIndex : Int -> ( Int, a ) -> Bool
hasIndex targetIndex indexedItem =
    let
        ( index, _ ) =
            indexedItem
    in
    targetIndex == index


toSet : Int -> Maybe Int -> Set Int
toSet index maybeBitset =
    case maybeBitset of
        Just bitset ->
            let
                offset =
                    index * intSize
            in
            List.range 0 intSize
                |> List.filterMap
                    (\i ->
                        if bitIsSet (Bitwise.shiftLeftBy i 1) bitset then
                            Just (i + offset)

                        else
                            Nothing
                    )
                |> Set.fromList

        Nothing ->
            Set.empty


bitIsSet : Int -> Int -> Bool
bitIsSet bit bitset =
    Bitwise.and bit bitset == bit
