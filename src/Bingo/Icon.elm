module Bingo.Icon exposing
    ( copy
    , image
    , plus
    , random
    , save
    )

import Html exposing (Html)
import Html.Attributes as Html


random : Html msg
random =
    icon "random"


plus : Html msg
plus =
    icon "plus"


save : Html msg
save =
    icon "save"


image : Html msg
image =
    icon "image"


copy : Html msg
copy =
    icon "copy"


icon : String -> Html msg
icon name =
    Html.span [ Html.class ("fas fa-" ++ name) ] []
