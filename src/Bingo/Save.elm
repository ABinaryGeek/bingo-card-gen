port module Bingo.Save exposing (save)


port saveOut : String -> Cmd msg


save : String -> Cmd msg
save name =
    saveOut name
