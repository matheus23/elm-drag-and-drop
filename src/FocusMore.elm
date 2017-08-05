module FocusMore exposing (..)

import Focus exposing (..)


type alias FieldSetter record field =
    Setter record record field field


first : Setter ( a, c ) ( b, c ) a b
first f ( x, y ) =
    ( f x, y )


second : Setter ( c, a ) ( c, b ) a b
second f ( x, y ) =
    ( x, f y )


index : Int -> FieldSetter (List a) a
index index f list =
    let
        applyAtIndex i elem =
            if i == index then
                f elem
            else
                elem
    in
    List.indexedMap applyAtIndex list


indexConcat : Int -> Setter (List a) (List a) a (List a)
indexConcat index f list =
    let
        applyAtIndex i elem =
            if i == index then
                f elem
            else
                [ elem ]
    in
    List.concat (List.indexedMap applyAtIndex list)


when : Bool -> (a -> a) -> a -> a
when shouldApply setter sth =
    if shouldApply then
        setter sth
    else
        sth
