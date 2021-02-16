module Benchmarks.Foldl exposing (main)

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
            List.range 0 999 |> Array.fromList

        sampleLisarray =
            List.foldl (\i -> SkewForest.cons i) [] (List.range 0 999)
    in
    Benchmark.describe "foldl"
        [ Benchmark.compare
            "Array.foldl vs SkewForest.foldl"
            "Array"
            (\_ -> Array.foldl (+) 0 sampleArray)
            "SkewForest"
            (\_ -> SkewForest.foldl (+) 0 sampleLisarray)
        ]


suite2 : Benchmark
suite2 =
    let
        sampleList =
            List.range 0 999

        sampleLisarray =
            List.foldl (\i -> SkewForest.cons i) [] (List.range 0 999)
    in
    Benchmark.describe "foldl"
        [ Benchmark.compare
            "List.foldl vs SkewForest.foldl"
            "List"
            (\_ -> List.foldl (+) 0 sampleList)
            "SkewForest"
            (\_ -> SkewForest.foldl (+) 0 sampleLisarray)
        ]
