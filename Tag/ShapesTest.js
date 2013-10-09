//
// Tests!
//

var  deps = [
    'Shapes.js',
    'Assert.js'
];

function onReady (Shapes, Assert) {

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

    var actual = Shapes.circle({radius: 50, color: 'red'});

    Assert.assertEq(actual.name, expected.name);
    Assert.assertEq(actual.style, expected.style);

    Yoink.define(actual);
}

Yoink.require(deps, onReady);
    
