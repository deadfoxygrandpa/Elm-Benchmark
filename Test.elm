module Benchmark where

import Array
import List (..)

type alias BenchmarkTest = () -> ()

discard : a -> ()
discard _ = ()

index : Int -> List a -> a
index n = head << drop n

append : List a -> List a -> List a
append a b = a ++ b

set : Int -> a -> List a -> List a
set n v xs = (take n xs) ++ [v] ++ (drop (n+1) xs)

list = [1..10]
list2 = [1..100]
list3 = [1..1000]

array = Array.fromList list
array2 = Array.fromList list2
array3 = Array.fromList list3

port f1 : BenchmarkTest
port f1 = \_ -> discard <| set 9 0 list

port t1 : BenchmarkTest
port t1 = \_ -> discard <| Array.set 9 0 array

port f2 : BenchmarkTest
port f2 = \_ -> discard <| set 90 0 list2

port t2 : BenchmarkTest
port t2 = \_ -> discard <| Array.set 90 0 array2

port f3 : BenchmarkTest
port f3 = \_ -> discard <| set 990 0 list3

port t3 : BenchmarkTest
port t3 = \_ -> discard <| Array.set 990 0 array3
