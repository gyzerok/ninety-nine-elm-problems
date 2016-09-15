module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Chapter1
import Chapter2
import Chapter3
import Chapter4


all : Test
all =
    describe "Ninety-Nine Elm Problems Suit"
        [ chapter1suit
        , chapter2suit
        , chapter3suit
        , chapter4suit
        ]


chapter1suit : Test
chapter1suit =
    describe "Chapter1"
        [ test "last gets last element from the nonempty list" <|
            \_ ->
                Expect.equal (Chapter1.last [ 1, 2, 3 ]) (Just 3)
        , test "last gets nothing from the empty list" <|
            \_ ->
                Expect.equal (Chapter1.last []) Nothing
        , test "butLast gets previous to last element from the nonempty list" <|
            \_ ->
                Expect.equal (Chapter1.butLast [ 1, 2, 3 ]) (Just 2)
        , test "butLast gets nothing from the list with exact one element" <|
            \_ ->
                Expect.equal (Chapter1.butLast [ 1 ]) Nothing
        , test "butLast gets nothing from the empty list" <|
            \_ ->
                Expect.equal (Chapter1.butLast []) Nothing
        , test "elementAt works as expected" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 1 [ 1, 2 ]) (Just 2)
        , test "elementAt returns Nothing for wrong index" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 3 [ 1, 2 ]) Nothing
        , test "elementAt returns Nothing for empty list" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 0 []) Nothing
        , fuzz (Fuzz.list Fuzz.int) "length works like builtin" <|
            \list ->
                Expect.equal (Chapter1.length list) (List.length list)
        , fuzz (Fuzz.list Fuzz.int) "reverse works like builtin" <|
            \list ->
                Expect.equal (Chapter1.reverse list) (List.reverse list)
        , test "isPalindrome works for palindromes" <|
            \_ ->
                Expect.equal (Chapter1.isPalindrome [ 1, 2, 1 ]) True
        , test "isPalindrome works for nonpalindromes" <|
            \_ ->
                Expect.equal (Chapter1.isPalindrome [ 1, 2, 2 ]) False
        , test "flatten works" <|
            \_ ->
                let
                    nestedList =
                        Chapter1.Elems
                            [ Chapter1.Elem 1
                            , Chapter1.Elem 2
                            , Chapter1.Elems
                                [ Chapter1.Elem 3
                                , Chapter1.Elem 4
                                ]
                            ]
                in
                    Expect.equal
                        (Chapter1.flatten nestedList)
                        [ 1, 2, 3, 4 ]
        , test "compress make list unique" <|
            \_ ->
                Expect.equal (Chapter1.compress [ 1, 2, 2, 3, 3, 3 ]) [ 1, 2, 3 ]
        , test "compress preserve order" <|
            \_ ->
                Expect.equal
                    (Chapter1.compress [ 1, 3, 2, 3, 3, 2 ])
                    [ 1, 3, 2 ]
        , test "pack works" <|
            \_ ->
                Expect.equal
                    (Chapter1.pack [ 1, 2, 2, 3, 3, 3 ])
                    [ [ 1 ], [ 2, 2 ], [ 3, 3, 3 ] ]
        , test "encode works" <|
            \_ ->
                Expect.equal
                    (Chapter1.encode [ 1, 2, 2, 3, 3, 3 ])
                    [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
        ]


chapter2suit : Test
chapter2suit =
    describe "Chapter2"
        [ test "encode works" <|
            \_ ->
                Expect.equal
                    (Chapter2.encode [ 1, 2, 2, 3, 3, 3 ])
                    [ Chapter2.Single 1, Chapter2.Multiple 2 2, Chapter2.Multiple 3 3 ]
        , test "encode works" <|
            \_ ->
                Expect.equal
                    (Chapter2.decode [ Chapter2.Single 1, Chapter2.Multiple 2 2, Chapter2.Multiple 3 3 ])
                    [ 1, 2, 2, 3, 3, 3 ]
        , test "dupli works" <|
            \_ ->
                Expect.equal
                    (Chapter2.dupli [ 1, 2 ])
                    [ 1, 1, 2, 2 ]
        , test "repli works" <|
            \_ ->
                Expect.equal
                    (Chapter2.repli 2 [ 1, 2 ])
                    [ 1, 1, 2, 2 ]
        , test "dropEvery works" <|
            \_ ->
                Expect.equal
                    (Chapter2.dropEvery 2 [ 1, 2, 3, 4 ])
                    [ 1, 3 ]
        , test "splitAt works" <|
            \_ ->
                Expect.equal
                    (Chapter2.splitAt 2 [ 1, 2, 3, 4 ])
                    ( [ 1, 2 ], [ 3, 4 ] )
        , test "slice works" <|
            \_ ->
                Expect.equal
                    (Chapter2.slice 2 3 [ 1, 2, 3, 4 ])
                    [ 2, 3 ]
        , test "rotate works for positive direction" <|
            \_ ->
                Expect.equal
                    (Chapter2.rotate 1 [ 1, 2, 3, 4 ])
                    [ 2, 3, 4, 1 ]
        , test "rotate works for negative direction" <|
            \_ ->
                Expect.equal
                    (Chapter2.rotate -1 [ 1, 2, 3, 4 ])
                    [ 4, 1, 2, 3 ]
        , test "removeAt returns just value for nonempty list" <|
            \_ ->
                Expect.equal
                    (Chapter2.removeAt 2 [ 1, 2, 3 ])
                    ( Just 2, [ 1, 3 ] )
        , test "removeAt returns nothing for nempty list" <|
            \_ ->
                Expect.equal
                    (Chapter2.removeAt 2 [])
                    ( Nothing, [] )
        ]


chapter3suit : Test
chapter3suit =
    describe "Chapter3"
        [ test "insertAt works" <|
            \_ ->
                Expect.equal
                    (Chapter3.insertAt 1 2 [ 1, 3, 4 ])
                    [ 1, 2, 3, 4 ]
        , test "range works" <|
            \_ ->
                Expect.equal
                    (Chapter3.range 1 4)
                    [1..4]
        , test "combinations2 works" <|
            \_ ->
                Expect.equal
                    (Chapter3.combinations2 [ 1, 2, 3 ])
                    [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ] ]
        , test "combinations3 works" <|
            \_ ->
                Expect.equal
                    (Chapter3.combinations3 [ 1, 2, 3, 4, 5 ])
                    [ [ 1, 2, 3 ]
                    , [ 1, 2, 4 ]
                    , [ 1, 2, 5 ]
                    , [ 1, 3, 4 ]
                    , [ 1, 3, 5 ]
                    , [ 1, 4, 5 ]
                    , [ 2, 3, 4 ]
                    , [ 2, 3, 5 ]
                    , [ 2, 4, 5 ]
                    , [ 3, 4, 5 ]
                    ]
        , test "combinations works" <|
            \_ ->
                Expect.equal
                    (Chapter3.combinations 3 [ 1, 2, 3, 4, 5 ])
                    (Chapter3.combinations3 [ 1, 2, 3, 4, 5 ])
        , test "lsort works" <|
            \_ ->
                Expect.equal
                    (Chapter3.lsort [ [ 1, 2, 3 ], [ 1 ], [ 1, 2 ] ])
                    [ [ 1 ], [ 1, 2 ], [ 1, 2, 3 ] ]
        ]


chapter4suit : Test
chapter4suit =
    describe "Chapter4"
        [ test "isPrime works" <|
            \_ ->
                Expect.equal
                    (List.map Chapter4.isPrime [ 1, 2, 4, 7, 10, 401 ])
                    [ True, True, False, True, False, True ]
        , test "gcd works" <|
            \_ ->
                Expect.equal
                    (List.map
                        (\( x, y ) -> Chapter4.gcd x y)
                        [ ( 36, 63 ), ( -3, -6 ), ( -3, 6 ) ]
                    )
                    [ 9, 3, 3 ]
        , test "coprime works" <|
            \_ ->
                Expect.equal
                    (List.map
                        (\( x, y ) -> Chapter4.coprime x y)
                        [ ( 36, 63 ), ( 35, 64 ) ]
                    )
                    [ False, True ]
        , test "totientPhi works" <|
            \_ ->
                Expect.equal
                    (Chapter4.totientPhi 10)
                    4
        , test "primeFactors works" <|
            \_ ->
                Expect.equal
                    (Chapter4.primeFactors 315)
                    [ 3, 3, 5, 7 ]
        , test "primeFactorsMult works" <|
            \_ ->
                Expect.equal
                    (Chapter4.primeFactorsMult 315)
                    [ ( 3, 2 ), ( 5, 1 ), ( 7, 1 ) ]
        , test "totientPhi' works" <|
            \_ ->
                Expect.equal
                    (Chapter4.totientPhi' 10)
                    (Chapter4.totientPhi 10)
        , test "goldbach works" <|
            \_ ->
                Expect.equal
                    (Chapter4.goldbach 28)
                    (Just ( 5, 23 ))
        , test "goldbachList works" <|
            \_ ->
                Expect.equal
                    (Chapter4.goldbachList 9 20)
                    [ Just ( 3, 7 ), Just ( 5, 7 ), Just ( 3, 11 ), Just ( 3, 13 ), Just ( 5, 13 ), Just ( 3, 17 ) ]
        ]
