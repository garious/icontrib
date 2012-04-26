//
// Tests!
//

var  deps = [
    'Tag.js',
    'Assert.js'
];

function onReady (Tag, Assert) {

    Assert.assertEq(Tag.tag('p', 'hello'), {name: 'p', subelements: 'hello'});

    define('passed!');
}

require(deps, onReady);
    