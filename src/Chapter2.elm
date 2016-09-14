module Chapter2 exposing (..)

import Chapter1


type Item a
    = Single a
    | Multiple Int a


encode : List a -> List (Item a)
encode =
    Chapter1.pack
        >> List.map
            (\xs ->
                case xs of
                    [] ->
                        Debug.crash "This is impossible!"

                    x :: [] ->
                        Single x

                    x :: _ ->
                        Multiple (List.length xs) x
            )


decode : List (Item a) -> List a
decode =
    List.concatMap
        (\item ->
            case item of
                Single x ->
                    [ x ]

                Multiple count x ->
                    List.repeat count x
        )


dupli : List a -> List a
dupli =
    List.concatMap (List.repeat 2)


repli : Int -> List a -> List a
repli count =
    List.concatMap (List.repeat count)


dropEvery : Int -> List a -> List a
dropEvery count =
    List.indexedMap
        (\i x ->
            if i % count == 0 then
                [ x ]
            else
                []
        )
        >> List.concat


splitAt : Int -> List a -> ( List a, List a )
splitAt n list =
    ( List.take n list, List.drop n list )


slice : Int -> Int -> List a -> List a
slice i k =
    List.drop (i - 1)
        >> List.take (k - i + 1)


rotate : Int -> List a -> List a
rotate n list =
    let
        shift =
            n % List.length list

        ( start, end ) =
            splitAt shift list
    in
        end ++ start


removeAt : Int -> List a -> ( Maybe a, List a )
removeAt n list =
    let
        ( start, end ) =
            splitAt (n - 1) list
    in
        ( List.head end
        , start ++ Maybe.withDefault [] (List.tail end)
        )
