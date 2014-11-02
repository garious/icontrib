var deps = [
    '/stdlib/assert.js',
    'index.js'
];

function onReady(assert, div) {
    var inputValue = div.contents[0].attributes.value;
    var outputValue = div.contents[1].attributes.value;

    // Verify initial state.
    assert.eq(inputValue.get(), '0');
    assert.eq(outputValue.get(), '1');

    // Set the left value to 5 and verify the output
    // text box contains the incremented value.
    inputValue.set('5');
    assert.eq(inputValue.get(), '5');
    assert.eq(outputValue.get(), '6');

    define('passed!');
}

require(deps, onReady);

