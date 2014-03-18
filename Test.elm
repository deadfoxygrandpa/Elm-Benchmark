module Benchmark where

type BenchmarkTest = () -> ()

discard : a -> ()
discard _ = ()

port add : Int -> Int
port add = \_ -> discard <| 5 + 10
