module Bingo.Save exposing
    ( Out
    , save
    )


type alias Out msg =
    String -> Cmd msg


save : Out msg -> String -> Cmd msg
save outPort name =
    outPort name
