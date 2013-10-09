//
// Tests!
//

var  deps = [
    'Tag.js',
    'Assert.js'
];

function onReady (Tag, Assert) {

    var t = Tag.tag({name: 'p', contents: 'hello'});
    Assert.assertEq(t.name, 'p');
    Assert.assertEq(t.contents, 'hello');

    Yoink.define('passed!');
}

Yoink.require(deps, onReady);
    
