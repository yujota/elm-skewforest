module SkewForest exposing
    ( SkewForest
    , cons, foldl, foldr, reverse, head, tail, isEmpty, headAndTail
    , set, get
    )

{-| An immutable list. You can get/update an element at a particular index.
This original SML code is on
Chris Okasaki "Purely Functional Data Structures" Cambridge University Press (1999/7/1).
I referred this book to write this code.


# Types

@docs SkewForest


# List-like Functions

@docs cons, foldl, foldr, reverse, head, tail, isEmpty, headAndTail


# Array-like Functions

@docs set, get

-}


type Tree a
    = Leaf a
    | Node a (Tree a) (Tree a)


{-| Representation of skew-binary random access list.
-}
type alias SkewForest a =
    List ( Int, Tree a )


{-| Create an empty forest.
-}
empty : SkewForest a
empty =
    []


{-| Check if a forest is empty.
-}
isEmpty : SkewForest a -> Bool
isEmpty =
    List.isEmpty


{-| Create a forest with only one element.
-}
singleton : a -> SkewForest a
singleton x =
    [ ( 1, Leaf x ) ]


{-| Add an element to the front of a forest.
-}
cons : a -> SkewForest a -> SkewForest a
cons x ts =
    case ts of
        ( w1, t1 ) :: ( w2, t2 ) :: ts_prime ->
            if w1 == w2 then
                ( 1 + w1 + w2, Node x t1 t2 ) :: ts_prime

            else
                ( 1, Leaf x ) :: ts

        _ ->
            ( 1, Leaf x ) :: ts


{-| Extract the first element from a forest
-}
head : SkewForest a -> Maybe a
head ts =
    case ts of
        [] ->
            Nothing

        ( 1, Leaf x ) :: _ ->
            Just x

        -- This case is not possible
        ( _, Leaf x ) :: _ ->
            Nothing

        ( _, Node x _ _ ) :: _ ->
            Just x


{-| Extract the rest of a forest
-}
tail : SkewForest a -> Maybe (SkewForest a)
tail ts =
    case ts of
        [] ->
            Nothing

        ( 1, Leaf _ ) :: ts_prime ->
            Just ts_prime

        -- This case is not possible
        ( _, Leaf x ) :: _ ->
            Nothing

        ( w, Node _ t1 t2 ) :: ts_prime ->
            Just <| ( w // 2, t1 ) :: ( w // 2, t2 ) :: ts_prime


{-| This function is equivalent to `\forest -> Maybe.map2 Tuple.pair (head forest) (tail forest)`.
-}
headAndTail : SkewForest a -> Maybe ( a, SkewForest a )
headAndTail ts =
    case ts of
        [] ->
            Nothing

        ( 1, Leaf x ) :: ts_prime ->
            Just ( x, ts_prime )

        -- This case is not possible
        ( _, Leaf x ) :: _ ->
            Nothing

        ( w, Node x t1 t2 ) :: ts_prime ->
            Just <| Tuple.pair x <| ( w // 2, t1 ) :: ( w // 2, t2 ) :: ts_prime


{-| map
-}
map : (a -> b) -> SkewForest a -> SkewForest b
map f ts =
    foldr (\x acc -> cons (f x) acc) empty ts


{-| foldl
-}
foldl : (a -> b -> b) -> b -> SkewForest a -> b
foldl func acc ts =
    case headAndTail ts of
        Just ( x, xs ) ->
            foldl func (func x acc) xs

        _ ->
            acc


{-| foldr
-}
foldr : (a -> b -> b) -> b -> SkewForest a -> b
foldr func acc ts =
    reverse ts |> foldl func acc


{-| reverse
-}
reverse : SkewForest a -> SkewForest a
reverse ts =
    foldl cons empty ts


{-| Get element at the index.
-}
get : Int -> SkewForest a -> Maybe a
get i ts =
    case ts of
        [] ->
            Nothing

        ( w, t ) :: ts_prime ->
            if i < w then
                getTree w i t

            else
                get (i - w) ts_prime


getTree : Int -> Int -> Tree a -> Maybe a
getTree w i tree =
    case ( w, i, tree ) of
        ( 1, 0, Leaf x ) ->
            Just x

        ( _, _, Leaf x ) ->
            Nothing

        ( _, 0, Node x _ _ ) ->
            Just x

        ( _, _, Node _ t1 t2 ) ->
            if i <= w // 2 then
                getTree (w // 2) (i - 1) t1

            else
                getTree (w // 2) (i - 1 - w) t2


{-| Set element at the index.
-}
set : Int -> a -> SkewForest a -> SkewForest a
set i y ts =
    case ts of
        [] ->
            ts

        ( w, t ) :: ts_prime ->
            if i < w then
                setTree w i y t
                    |> Maybe.map (\tp -> ( w, tp ) :: ts_prime)
                    |> Maybe.withDefault ts

            else
                ( w, t ) :: set (i - w) y ts


setTree : Int -> Int -> a -> Tree a -> Maybe (Tree a)
setTree w i y tree =
    case ( w, i, tree ) of
        ( 1, 0, Leaf x ) ->
            Just (Leaf y)

        ( _, _, Leaf _ ) ->
            Nothing

        ( _, 0, Node _ t1 t2 ) ->
            Just (Node y t1 t2)

        ( _, _, Node x t1 t2 ) ->
            if i <= w // 2 then
                setTree (w // 2) (i - 1) y t1
                    |> Maybe.map (\t1p -> Node x t1p t2)

            else
                setTree (w // 2) (i - 1 - w) y t2
                    |> Maybe.map (\t2p -> Node x t1 t2p)
