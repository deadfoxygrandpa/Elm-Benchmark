Benchmark.js in Elm
=========

This repo provides a library for writing console-based Benchmark.js tests in
Elm.

Getting Started
---------------

Before you begin, you must pull in both the node.js dependencies (jsdom and benchmark):

```bash
$ cabal install
$ npm install benchmark
$ npm install jsdom
```

(On Windows, `jsdom` is somewhat difficult to install. [Refer to this blog post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/) for detailed instructions)

Example
-------
Each test must be an outgoing `port` of type `() -> ()`. Define any number
of these `() -> ()` tests in a module named `Benchmark`. Here's an example:
```haskell
-- Test.elm
module Benchmark where

port testAdd10 : () -> ()
port testAdd10 = \n -> n + 10
```
That's all the Elm you need. Compile and run it with:
```bash
$ elm-benchmark Test.elm test.js
Generating JavaScript ... Done
Making exe

$ node test.js
testAdd10 x 22,790,539 ops/sec ±0.31% (98 runs sampled)
```
And you can see the benchmark test results.

For more complicated functions, you can set them up as functions that take a dummy
`Int` and return a dummy `Int`, as we don't care about the actual value. Elm is
strictly evaluated, so even if we discard the result of a function call, it will
still execute. Here's an example:
```haskell
module Benchmark where

import Dict

-- Helper function to discard results
discard : a -> Int
discard _ = 0

-- Test setup
list = zip [1..1000] [1..1000]

-- Benchmark tests:
port testDictFromList : () -> ()
port testDictFromList = \_ -> discard . Dict.fromList <| list
```
Again, compile and run it with:
```bash
$ elm-benchmark Test.elm test.js
Generating JavaScript ... Done
Making exe

$ node test.js
testDictFromList x 50.91 ops/sec ±0.37% (68 runs sampled)
```

Command Line Interface
----------------------
The basic interface is `elm-benchmark infile outfile`, where `infile` is an
Elm source file to compile and `outfile` is the resulting JavaScript
file to be run with node. The `infile` module MUST be `Benchmark`,
that is to say that the source file must have `module Benchmark where` as
the first line. This restriction may be lifted in the future.
