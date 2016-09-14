module Chapter1 exposing (..)


last : List a -> Maybe a
last =
    List.head << List.reverse


butLast : List a -> Maybe a
butLast =
    List.head << (List.drop 1) << List.reverse


elementAt : Int -> List a -> Maybe a
elementAt index =
    List.head << List.drop index


length : List a -> Int
length =
    List.foldr (\_ acc -> acc + 1) 0


reverse : List a -> List a
reverse =
    List.foldl (\x acc -> x :: acc) []


isPalindrome : List a -> Bool
isPalindrome list =
    List.map2 (,) list (List.reverse list)
        |> List.foldl
            (\( x, y ) acc ->
                if x == y then
                    acc
                else
                    False
            )
            True


type NestedList a
    = Elem a
    | Elems (List (NestedList a))


flatten : NestedList a -> List a
flatten list =
    case list of
        Elem x ->
            [ x ]

        Elems xs ->
            List.concatMap flatten xs


compress : List a -> List a
compress =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc
            else
                x :: acc
        )
        []


pack : List a -> List (List a)
pack list =
    let
        takeWhile p xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    if p y then
                        y :: takeWhile p ys
                    else
                        []

        dropWhile p xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    if p y then
                        dropWhile p ys
                    else
                        y :: ys
    in
        case list of
            [] ->
                []

            x :: xs ->
                (x :: takeWhile ((==) x) xs) :: pack (dropWhile ((==) x) xs)


encode : List a -> List ( Int, a )
encode =
    pack
        >> List.map
            (\xs ->
                case xs of
                    [] ->
                        Debug.crash "This is impossible!"

                    x :: _ ->
                        ( List.length xs, x )
            )
