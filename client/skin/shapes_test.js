//
// Tests!
//

var  deps = [
    'shapes.js',
    '/stdlib/assert.js'
];

function onReady (shapes, assert) {

    var expected = {
        name: 'div',
        style: {
            width: '100px',
            height: '100px',
            background: 'red',
            mozBorderRadius: '50px',
            webkitBorderRadius: '50px',
            borderRadius: '50px',
            position: 'relative'
        }
    };

    var actual = shapes.circle({radius: 50, color: 'red'});

    assert.assertEq(actual.name, expected.name);
    assert.assertEq(actual.style, expected.style);

    define(actual);
}

require(deps, onReady);
    
