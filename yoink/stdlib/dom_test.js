//
// Tests!
//

var  deps = [
    'dom.js',
    'assert.js'
];

function onReady (dom, assert) {

    //
    // Test 'element' and 'clone'.
    //
    (function() {
        var e = dom.element({name: 'p', contents: 'hello'});
        assert.eq(e.name, 'p');
        assert.eq(e.contents, 'hello');

        var e2 = dom.clone(e);
        e2.name = 'h1';
        assert.eq(e.name, 'p');  // Did not overwrite original.
        assert.eq(e2.name, 'h1');
        assert.eq(e2.contents, 'hello');
    })();

    //
    // Test 'mixin'.
    //
    (function() {
        var x = {a: 1, b: 2};
        var y = {a: 10, c: 30};
        var z = dom.mixin(x, y);
        assert.eq(z.a, 10);
        assert.eq(z.b, 2);
        assert.eq(z.c, 30);
    })();

    define('passed!');
}

require(deps, onReady);
    
