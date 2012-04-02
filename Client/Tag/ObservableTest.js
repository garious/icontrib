//
// Tests!
//

var  deps = [
    'Interface.js',
    'Observable.js',
    'Assert.js'
];

function onReady (Iface, Observable, Assert) {
    
    // Observable values without objects.
    var x = Observable.observe(5);
    var y = Observable.observe(6);

    // Peek at what's inside
    Assert.assertEq(x, {value: 5, constructor: Observable.observe, set: x.set});

    var methods = Iface.getInterface(x, Observable.observableId);
    Assert.assertEq(methods.get(x), 5);
    Assert.assertEq(methods.get(y), 6);

    x.set(3);
    Assert.assertEq(methods.get(x), 3);


    // Test thunk
    var rawAdd = function(a,b) {return a + b;};
    var comp = Observable.thunk([x, y], rawAdd);
    var compMethods = Iface.getInterface(comp, Observable.observableId);
    Assert.assertEq( compMethods.get(comp), 9 );

    x.set(5);
    Assert.assertEq( compMethods.get(comp), 11 );
    
    // Same as above, but using the lift() helper
    x.set(3);
    var add = Observable.lift(rawAdd);
    comp = add(x, y);
    Assert.assertEq( compMethods.get(comp), 9 );

    // As as above, but where a dependency is not an observable.
    comp = add(x, 6);
    Assert.assertEq( compMethods.get(comp), 9 );


    // Multi-level compuation
    comp = add(add(x, 5), 1);
    Assert.assertEq( compMethods.get(comp), 9 );

    x.set(5);

    // Note: the value has not changed yet.
    Assert.assertEq( comp.value, 9 );
    Assert.assertEq( comp.valid, false );

    // Call get() to update the computation tree
    Assert.assertEq( compMethods.get(comp), 11 );

    define('passed!');
}

require(deps, onReady);
    
