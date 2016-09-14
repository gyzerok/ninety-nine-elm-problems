module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Chapter1


all : Test
all =
    describe "Ninety-Nine Elm Problems Suit"
        [ chapter1suit
        ]


chapter1suit : Test
chapter1suit =
    describe "Chapter1"
        -- last
        [ test "last gets last element from the nonempty list" <|
            \_ ->
                Expect.equal (Chapter1.last [ 1, 2, 3 ]) (Just 3)
        , test "last gets nothing from the empty list" <|
            \_ ->
                Expect.equal (Chapter1.last []) Nothing
          -- butLast
        , test "butLast gets previous to last element from the nonempty list" <|
            \_ ->
                Expect.equal (Chapter1.butLast [ 1, 2, 3 ]) (Just 2)
        , test "butLast gets nothing from the list with exact one element" <|
            \_ ->
                Expect.equal (Chapter1.butLast [ 1 ]) Nothing
        , test "butLast gets nothing from the empty list" <|
            \_ ->
                Expect.equal (Chapter1.butLast []) Nothing
          -- elementAt
        , test "elementAt works as expected" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 1 [ 1, 2 ]) (Just 2)
        , test "elementAt returns Nothing for wrong index" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 3 [ 1, 2 ]) Nothing
        , test "elementAt returns Nothing for empty list" <|
            \_ ->
                Expect.equal (Chapter1.elementAt 0 []) Nothing
          -- length
        , fuzz (Fuzz.list Fuzz.int) "length works like builtin" <|
            \list ->
                Expect.equal (Chapter1.length list) (List.length list)
          -- reverse
        , fuzz (Fuzz.list Fuzz.int) "reverse works like builtin" <|
            \list ->
                Expect.equal (Chapter1.reverse list) (List.reverse list)
          -- isPalindrome
        , test "isPalindrome works for palindromes" <|
            \_ ->
                Expect.equal (Chapter1.isPalindrome [ 1, 2, 1 ]) True
        , test "isPalindrome works for nonpalindromes" <|
            \_ ->
                Expect.equal (Chapter1.isPalindrome [ 1, 2, 2 ]) False
          -- flatten
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
          -- compress
        , test "compress make list unique" <|
            \_ ->
                Expect.equal (Chapter1.compress [ 1, 2, 2, 3, 3, 3 ]) [ 1, 2, 3 ]
        , test "compress preserve order" <|
            \_ ->
                Expect.equal
                    (Chapter1.compress [ 1, 3, 2, 3, 3, 2 ])
                    [ 1, 3, 2 ]
          -- pack
        , test "pack works" <|
            \_ ->
                Expect.equal
                    (Chapter1.pack [ 1, 2, 2, 3, 3, 3 ])
                    [ [ 1 ], [ 2, 2 ], [ 3, 3, 3 ] ]
          -- encode
        , test "encode works" <|
            \_ ->
                Expect.equal
                    (Chapter1.encode [ 1, 2, 2, 3, 3, 3 ])
                    [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
        ]
