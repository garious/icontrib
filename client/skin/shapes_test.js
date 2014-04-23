//
// Tests!
//

var  deps = [
    'shapes.js',
    '/yoink/assert.js'
];

function onReady (shapes, Assert) {

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

    Assert.assertEq(actual.name, expected.name);
    Assert.assertEq(actual.style, expected.style);

    yoink.define(actual);
}

yoink.require(deps, onReady);
    
