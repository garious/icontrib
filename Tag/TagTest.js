//
// Tests!
//

var  deps = [
    'Tag.js',
    'Assert.js'
];

function onReady (Tag, Assert) {

    Assert.assertEq(Tag.tag({name: 'p', contents: 'hello'}), {name: 'p', contents: 'hello'});

    Yoink.define('passed!');
}

Yoink.require(deps, onReady);
    
