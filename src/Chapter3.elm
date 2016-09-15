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


combinations2 : List a -> List (List a)
combinations2 list =
    case list of
        [] ->
            []

        _ :: [] ->
            []

        x :: xs ->
            xs
                |> List.map (\y -> x :: y :: [])
                |> (flip (++)) (combinations2 xs)


combinations3 : List a -> List (List a)
combinations3 list =
    case list of
        [] ->
            []

        _ :: [] ->
            []

        _ :: _ :: [] ->
            []

        x :: xs ->
            combinations2 xs
                |> List.map (\ys -> x :: ys)
                |> (flip (++)) (combinations3 xs)


combinations : Int -> List a -> List (List a)
combinations k list =
    case ( k, list ) of
        ( _, [] ) ->
            []

        ( 0, _ ) ->
            []

        ( 1, _ ) ->
            List.map (\x -> [ x ]) list

        ( _, x :: xs ) ->
            combinations (k - 1) xs
                |> List.map (\ys -> x :: ys)
                |> (flip (++)) (combinations k xs)


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
