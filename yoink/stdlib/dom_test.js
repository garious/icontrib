//
// Tests!
//

var  deps = [
    'dom.js',
    'assert.js'
];

function onReady (dom, assert) {

    var t = dom.element({name: 'p', contents: 'hello'});
    assert.assertEq(t.name, 'p');
    assert.assertEq(t.contents, 'hello');

    define('passed!');
}

require(deps, onReady);
    
