module Bingo.Viewer.Stamps exposing
    ( Stamp
    , Stamps
    , add
    , atIndex
    , decoder
    , empty
    , encode
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


{-| A set of stamps.
Hidden behind a type so it's impossible to accidentally use the wrong int.
-}
type Stamps
    = BitArray Int


{-| A stamp at a given index.
Hidden behind a type so it's impossible to accidentally use the wrong int.
-}
type Stamp
    = Bit Int


{-| Get a stamp representing a given index.
-}
atIndex : Int -> Stamp
atIndex index =
    Bit (Bitwise.shiftLeftBy index 1)


{-| An empty stamp set.
-}
empty : Stamps
empty =
    BitArray 0


{-| Add the given stamp to the set.
-}
add : Stamp -> Stamps -> Stamps
add stamp stamps =
    BitArray (Bitwise.or (bit stamp) (bitArray stamps))


{-| Remove the given stamp from the set.
-}
remove : Stamp -> Stamps -> Stamps
remove stamp stamps =
    BitArray (Bitwise.and (bitArray stamps) (Bitwise.complement (bit stamp)))


{-| Returns true if the stamp is in the set.
-}
isSet : Stamp -> Stamps -> Bool
isSet stamp stamps =
    let
        stampBit =
            bit stamp
    in
    Bitwise.and stampBit (bitArray stamps) == stampBit


{-| Adds the stamp if it isn't in the set, removes it if it is.
-}
toggle : Stamp -> Stamps -> Stamps
toggle stamp stamps =
    BitArray (Bitwise.xor (bit stamp) (bitArray stamps))


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
    Json.Encode.int (bitArray stamps)


{-| Decoder for a stamps set from JSON.
-}
decoder : Json.Decode.Decoder Stamps
decoder =
    Json.Decode.int |> Json.Decode.map BitArray



{- Private -}


bit : Stamp -> Int
bit stamp =
    case stamp of
        Bit b ->
            b


bitArray : Stamps -> Int
bitArray stamps =
    case stamps of
        BitArray ba ->
            ba
