module Helper exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


log : List String -> b -> b
log strs =
    Debug.log (unwords strs) ()
        |> (always identity)


unwords =
    List.foldr (++) "" << List.intersperse " "


unlines =
    List.foldr (++) "" << List.intersperse "\n"


bracket x =
    "(" ++ x ++ ")"


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


generateWrite : (a -> ( Maybe a, b )) -> a -> Nonempty b
generateWrite gen =
    let
        go : List b -> a -> Nonempty b
        go bs a =
            case gen a of
                ( Nothing, b ) ->
                    Nonempty b bs

                ( Just next, b ) ->
                    go (b :: bs) next
    in
        Nonempty.reverse << go []


generate : (a -> Maybe a) -> a -> Nonempty a
generate gen =
    let
        go : List a -> a -> Nonempty a
        go xs x =
            case gen x of
                Nothing ->
                    Nonempty x xs

                Just next ->
                    go (x :: xs) next
    in
        Nonempty.reverse << go []


toNonemptyUnsafe xs =
    case Nonempty.fromList xs of
        Just xs ->
            xs

        Nothing ->
            Debug.crash <|
                unwords
                    [ "Cannot turn empty list"
                    , "into nonempty list"
                    ]


sequenceMaybes : List (Maybe a) -> Maybe (List a)
sequenceMaybes =
    List.foldr (Maybe.map2 (::)) (Just [])


sequenceMaybesNonempty : Nonempty (Maybe a) -> Maybe (Nonempty a)
sequenceMaybesNonempty =
    nonemptyFoldr (Maybe.map2 (:::)) (Maybe.map Nonempty.fromElement)


nonemptyFoldr : (a -> b -> b) -> (a -> b) -> Nonempty a -> b
nonemptyFoldr cons last =
    Nonempty.reverse
        >> (\(Nonempty x xs) -> List.foldl cons (last x) xs)
