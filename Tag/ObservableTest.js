//
// Tests!
//

var  deps = [
    'Observable.js',
    'Assert.js'
];

function onReady (Observable, Assert) {
    
    // Observable values without objects.
    var x = Observable.observe(5);
    var y = Observable.observe(6);

    Assert.assertEq(x.get(), 5);
    Assert.assertEq(y.get(), 6);

    x.set(3);
    Assert.assertEq(x.get(), 3);


    // Test thunk
    var rawAdd = function(a,b) {return a + b;};
    var comp = Observable.thunk([x, y], rawAdd);
    Assert.assertEq( comp.get(), 9 );

    x.set(5);
    Assert.assertEq( comp.get(), 11 );
    
    // Same as above, but using the lift() helper
    x.set(3);
    var add = Observable.lift(rawAdd);
    comp = add(x, y);
    Assert.assertEq( comp.get(), 9 );

    // As as above, but where a dependency is not an observable.
    comp = add(x, 6);
    Assert.assertEq( comp.get(), 9 );


    // Multi-level compuation
    comp = add(add(x, 5), 1);
    Assert.assertEq( comp.get(), 9 );

    x.set(5);

    // Note: the value has not changed yet.
    Assert.assertEq( comp.value, 9 );
    Assert.assertEq( comp.valid, false );

    // Call get() to update the computation tree
    Assert.assertEq( comp.get(), 11 );

    Yoink.define('passed!');
}

Yoink.require(deps, onReady);
    
