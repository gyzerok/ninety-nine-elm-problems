module Chapter4 exposing (..)

import Chapter1


isPrime : Int -> Bool
isPrime x =
    if x == 1 then
        True
    else
        [1..(x // 2)]
            |> List.filter (\y -> x % y == 0)
            |> List.length
            |> (==) 1


gcd : Int -> Int -> Int
gcd x y =
    if y == 0 then
        x
    else if x < 0 then
        gcd -x y
    else if y < 0 then
        gcd x -y
    else
        gcd y (x % y)


coprime : Int -> Int -> Bool
coprime x y =
    gcd x y == 1


totientPhi : Int -> Int
totientPhi x =
    [1..x]
        |> List.filter (\y -> coprime x y)
        |> List.length


primeFactors : Int -> List Int
primeFactors x =
    if x == 1 then
        []
    else
        let
            maybePrimeFactor =
                [2..x]
                    |> List.filter isPrime
                    |> List.filter (\y -> x % y == 0)
                    |> List.head
        in
            case maybePrimeFactor of
                Just primeFactor ->
                    primeFactor :: primeFactors (x // primeFactor)

                Nothing ->
                    []


primeFactorsMult : Int -> List ( Int, Int )
primeFactorsMult x =
    primeFactors x
        |> Chapter1.encode
        |> List.map (\( a, b ) -> ( b, a ))


totientPhi' : Int -> Int
totientPhi' x =
    primeFactorsMult x
        |> List.foldr (\( factor, multiplier ) acc -> acc * (factor - 1) * factor ^ (multiplier - 1)) 1


goldbach : Int -> Maybe ( Int, Int )
goldbach x =
    [2..x]
        |> List.filter isPrime
        |> List.filter (\y -> isPrime (x - y))
        |> List.head
        |> Maybe.map (\y -> ( y, x - y ))


goldbachList : Int -> Int -> List (Maybe ( Int, Int ))
goldbachList x y =
    [x..y]
        |> List.filter (\a -> a % 2 == 0)
        |> List.map goldbach
