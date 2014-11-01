//
// Interface tests
//

var deps = [
    'assert.js'
];

function onReady(assert) {

    var eq = assert.assertEq;
    function neq(a, b) {
        try {
            eq(a, b);
        } catch(err) {
            return err;
        }
        throw "Expected not equal, but were equal";
    }

    eq(false, false);
    eq(true, true);

    eq(0, 0);
    eq(1, 1);

    eq('', '');
    eq('a', 'a');
    neq(1, '1');

    eq([], []);
    eq([1,2,3], [1,2,3]);
    neq([1,2,3], [1,2]);
    neq([1,2], [1,2,3]);

    eq( {},     {});
    eq( {a: 1}, {a: 1});
    neq({a: 1}, {});
    neq({},     {a: 1});


    define('passed!');
}

require(deps, onReady);

