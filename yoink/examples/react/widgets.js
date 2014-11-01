var deps = [
    '/stdlib/dom.js'
];

function onReady(dom) {

    function numInput(v, readOnly) {
        return dom.element({
            name: 'input',
            attributes: {
                type: 'number',
                value: v,
                readOnly: readOnly
            }
        });
    }

    function box(contents) {
        return dom.element({
            name: 'div',
            contents: contents
        });
    }

    define({
        numInput: numInput,
        box: box
    });
}

require(deps, onReady);

