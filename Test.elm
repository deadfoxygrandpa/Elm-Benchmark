module Benchmark where

type BenchmarkTest = () -> ()

discard : a -> ()
discard _ = ()

port add : () -> ()
port add = \_ -> discard <| 5 + 10
