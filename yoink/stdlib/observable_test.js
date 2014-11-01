//
// Tests!
//

var  deps = [
    'observable.js',
    'assert.js'
];

function onReady (observable, assert) {

    // Observable values without objects.
    var x = observable.publisher(5);
    var y = observable.publisher(6);

    assert.assertEq(x.get(), 5);
    assert.assertEq(y.get(), 6);

    x.set(3);
    assert.assertEq(x.get(), 3);


    // Test subscriber
    var rawAdd = function(a,b) {return a + b;};
    var comp = observable.subscriber([x, y], rawAdd);
    assert.assertEq(comp.set, undefined);
    assert.assertEq( comp.get(), 9 );

    x.set(5);
    assert.assertEq( comp.get(), 11 );
    
    // Same as above, but using the lift() helper
    x.set(3);
    var add = observable.lift(rawAdd);
    comp = add(x, y);
    assert.assertEq( comp.get(), 9 );

    // As as above, but where a dependency is not an observable.
    comp = add(x, 6);
    assert.assertEq( comp.get(), 9 );


    // Multi-level compuation
    comp = add(add(x, 5), 1);
    assert.assertEq( comp.get(), 9 );

    x.set(5);

    // Note: the value has not changed yet.
    assert.assertEq( comp.value, 9 );
    assert.assertEq( comp.valid, false );

    // Call get() to update the computation tree
    assert.assertEq( comp.get(), 11 );

    // Verify observables are instances of Observable
    assert.assertEq(x instanceof observable.Observable, true);

    // Verify subscribers are instances of Observable
    assert.assertEq(comp instanceof observable.Observable, true);

    define('passed!');
}

require(deps, onReady);
    
