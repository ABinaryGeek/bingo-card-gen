module Bingo.Icon exposing
    ( copy
    , edit
    , fileImport
    , image
    , listUl
    , plus
    , questionCircle
    , random
    , save
    , timesCircle
    , trash
    )

import Html exposing (Html)
import Html.Attributes as Html


copy : Html msg
copy =
    icon "copy"


edit : Html msg
edit =
    icon "edit"


fileImport : Html msg
fileImport =
    icon "file-import"


image : Html msg
image =
    icon "image"


listUl : Html msg
listUl =
    icon "list-ul"


plus : Html msg
plus =
    icon "plus"


questionCircle : Html msg
questionCircle =
    icon "question-circle"


random : Html msg
random =
    icon "random"


save : Html msg
save =
    icon "save"


timesCircle : Html msg
timesCircle =
    icon "times-circle"


trash : Html msg
trash =
    icon "trash"



{- Private -}


icon : String -> Html msg
icon name =
    Html.span [ Html.class ("fas fa-" ++ name) ] []
