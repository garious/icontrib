var deps = [
    'nav.js',
    'core.js'
];

function onReady(Nav, Core) {

    var node = Nav.footer({style: {backgroundColor: '#EEE'}}, [
        Core.hyperlink({url: '#', text: 'blah'})
    ]);

    define(node);
}

require(deps, onReady);

