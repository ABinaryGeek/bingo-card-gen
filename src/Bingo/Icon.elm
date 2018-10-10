module Bingo.Icon exposing
    ( copy
    , edit
    , fileImport
    , github
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
    solidIcon "copy"


edit : Html msg
edit =
    solidIcon "edit"


fileImport : Html msg
fileImport =
    solidIcon "file-import"


github : Html msg
github =
    brandIcon "github"


image : Html msg
image =
    solidIcon "image"


listUl : Html msg
listUl =
    solidIcon "list-ul"


plus : Html msg
plus =
    solidIcon "plus"


questionCircle : Html msg
questionCircle =
    solidIcon "question-circle"


random : Html msg
random =
    solidIcon "random"


save : Html msg
save =
    solidIcon "save"


timesCircle : Html msg
timesCircle =
    solidIcon "times-circle"


trash : Html msg
trash =
    solidIcon "trash"



{- Private -}


icon : String -> String -> Html msg
icon set name =
    Html.span [ Html.class (set ++ " fa-" ++ name) ] []


solidIcon : String -> Html msg
solidIcon =
    icon "fas"


brandIcon : String -> Html msg
brandIcon =
    icon "fab"
