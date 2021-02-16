module Benchmarks.Head exposing (main)

import Array exposing (Array)
import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SkewForest


main : BenchmarkProgram
main =
    program suite2


suite : Benchmark
suite =
    let
        sampleArray =
            Array.initialize 1000 identity

        sampleLisarray =
            List.foldl (\_ -> SkewForest.cons identity) [] (List.repeat 1000 identity)
    in
    Benchmark.describe "head"
        [ Benchmark.compare
            "Array.get 0 vs SkewForest.head"
            "Array"
            (\_ -> Array.get 0 sampleArray)
            "SkewForest"
            (\_ -> SkewForest.head sampleLisarray)
        ]


suite2 : Benchmark
suite2 =
    let
        sampleList =
            List.repeat 1000 identity

        sampleLisarray =
            List.foldl (\_ -> SkewForest.cons identity) [] (List.repeat 1000 identity)
    in
    Benchmark.describe "head"
        [ Benchmark.compare
            "List.head vs SkewForest.head"
            "List"
            (\_ -> List.head sampleList)
            "SkewForest"
            (\_ -> SkewForest.head sampleLisarray)
        ]
