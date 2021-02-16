module Benchmarks.Get exposing (main)

import Array exposing (Array)
import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SkewForest


main : BenchmarkProgram
main =
    program suite3


suite : Benchmark
suite =
    let
        sampleArray =
            Array.initialize 1000 identity

        sampleLisarray =
            List.foldl (\_ -> SkewForest.cons identity) [] (List.repeat 1000 identity)
    in
    Benchmark.describe "get"
        [ Benchmark.compare
            "Array.get vs SkewForest.get"
            "Array"
            (\_ -> Array.get 182 sampleArray)
            "SkewForest"
            (\_ -> SkewForest.get 182 sampleLisarray)
        ]


suite2 : Benchmark
suite2 =
    let
        sampleList =
            List.repeat 1000 identity

        sampleLisarray =
            List.foldl (\_ -> SkewForest.cons identity) [] (List.repeat 1000 identity)

        getFromList i maybeXs =
            case maybeXs of
                Just xs ->
                    if i > 0 then
                        getFromList (i - 1) (List.tail xs)

                    else
                        List.head xs

                _ ->
                    Nothing
    in
    Benchmark.describe "get"
        [ Benchmark.compare
            "List get-like func vs SkewForest.get"
            "List"
            (\_ -> getFromList 82 (Just sampleList))
            "SkewForest"
            (\_ -> SkewForest.get 82 sampleLisarray)
        ]


suite3 : Benchmark
suite3 =
    let
        sampleList =
            List.repeat 1000 identity

        sampleLisarray =
            List.foldl (\_ -> SkewForest.cons identity) [] (List.repeat 1000 identity)

        getFromList i xs =
            Array.fromList xs |> Array.get i
    in
    Benchmark.describe "get"
        [ Benchmark.compare
            "Array.fromList |> Array.get vs SkewForest.get"
            "List"
            (\_ -> getFromList 82 sampleList)
            "SkewForest"
            (\_ -> SkewForest.get 82 sampleLisarray)
        ]
