//
// Interface tests
//

var deps = [
    'Interface.js',
    'Assert.js'
];

function onReady(Iface, Assert) {

    function assertSupports(o, i) {
        Assert.assertEq(Iface.supportsInterface(o, i), true);
    }

    function assertNotSupports(o, i) {
        Assert.assertEq(Iface.supportsInterface(o, i), false);
    }

    function a(){}
    function b(x){}
    function c() {}
    c.prototype.a = a;

    assertSupports(    {a: a},        {a: function(){}});  // o obviously implements i
    assertNotSupports( {a: a},        {a: function(x){}}); // arity mismatch
    assertNotSupports( {a: a},        undefined);          // absurd
    assertSupports(    {a: a},        {a: true});          // interface types don't need to be functions
    assertNotSupports( {a: a},        {a: true, b: true}); // object methods must be a subset of interface methods
    assertSupports(    {a: a, b: b},  {a: function() {}}); // object methods need only be a subset of interface methods
    assertSupports(    new c(),       {a: function() {}}); // o implements i if its prototype implements i

    Yoink.define('passed!');
}

Yoink.require(deps, onReady);

