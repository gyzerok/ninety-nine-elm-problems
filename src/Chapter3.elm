module Chapter3 exposing (..)


insertAt : Int -> a -> List a -> List a
insertAt index element list =
    let
        start =
            List.take index list

        end =
            List.drop index list
    in
        start ++ [ element ] ++ end


range : Int -> Int -> List Int
range a b =
    [a..b]


combinations2 : List a -> List ( a, a )
combinations2 list =
    case list of
        [] ->
            []

        x :: [] ->
            []

        x :: xs ->
            List.map2 (,)
                (List.repeat (List.length xs) x)
                xs
                |> (flip (++)) (combinations2 xs)


group : Int -> List a -> List (List a)
group count list =
    Debug.crash "not implemented"


lsort : List (List a) -> List (List a)
lsort list =
    case list of
        [] ->
            []

        xs :: xss ->
            let
                ( left, right ) =
                    List.partition (\ys -> List.length ys <= List.length xs) xss
            in
                lsort left ++ [ xs ] ++ lsort right
