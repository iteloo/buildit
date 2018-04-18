module Helper exposing (..)


generate : (a -> Maybe a) -> a -> List a
generate gen =
    let
        go : List a -> a -> List a
        go xs x =
            case gen x of
                Nothing ->
                    x :: xs

                Just next ->
                    go (x :: xs) next
    in
        List.reverse << go []
