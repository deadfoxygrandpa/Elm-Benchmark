
// Elm goes there ^
(function(){
    var worker = Elm.worker(Elm.Benchmark);

    var Benchmark = require('benchmark');

    var suite = new Benchmark.Suite;

    suite.on('cycle', function(event) {
      console.log(String(event.target));
    });

    var makeTest = function(x) { return function () { x(0); } };

    for (var key in worker.ports) {
        var name = key.toString();
        suite.add(name, makeTest( eval("worker.ports." + name) ));
    }
    
    suite.run();

})();
} // Close the callback
// Run!
jsdom.env('<p>bleh</p>', [], callback);
