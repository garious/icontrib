//
// Tests!
//

var  deps = [
    'Tag.js',
    'Assert.js'
];

function onReady (Tag, Assert) {

    Assert.assertEq(Tag.tag('p', 'hello'), {name: 'p', subelements: 'hello'});

    Yoink.define('passed!');
}

Yoink.require(deps, onReady);
    
