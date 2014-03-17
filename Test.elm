module Benchmark where

type BenchmarkTest = Int -> Int

port add : Int -> Int
port add = \_ -> 5 + 10