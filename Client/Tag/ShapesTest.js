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
        attributes: {
            style: {
                width: '100px',
                height: '100px',
                background: 'red',
                mozBorderRadius: '50px',
                webkitBorderRadius: '50px',
                borderRadius: '50px'
            }
        }
    };

    var actual = Shapes.circle({radius: 50, color: 'red'});

    Assert.assertEq(actual, expected);

    define(actual);
}

require(deps, onReady);
    
