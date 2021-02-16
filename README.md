# elm-skewforest

An elm implementation of skew-binary random access list.
This package provides both list-like APIs such as `cons, head, tail` and
array-like APIs such as `get, set` for the data type `SkewForest`.

The original SML code is on Chris Okasaki "Purely Functional Data Structures" Cambridge University Press (1999/7/1).
I referred this book to write this code.

```elm
forest = SkewForest.empty |> SkewForest.cons 'a' |> SkewForest.cons 'b'
firstElem = SkewForest.head forest  -- Just 'a'
secondElem = SkewForest.get 1 forest -- Just 'b'


someRecursiveFunc : Acc -> SkewForest a -> Acc
someRecursiveFunc acc forest =
    case SkewForest.headAndTail forest of
        Just (head, tail) ->
            someRecursiveFunc (doSomething acc forest) tail
        Nothing -> 
            acc
```

## Performance

When the number of element is 1000, `SkewForest.get` function is roughly two times slower than 
`Array.get` but faster than converting a list to an array and then executing `Array.get`.


## When to use?

In most of the cases, you can't find any benefit to use this code.
If you want to use both `head/tail` and `get/set` function for a sequential data, this code could be an option.
