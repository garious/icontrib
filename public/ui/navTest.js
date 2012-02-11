var deps = [
    'nav.js',
    'core.js',
    '/tag/tag.js'
];

function onReady(NAV, CORE, E) {

    var node = NAV.footer({style: {backgroundColor: '#EEE'}}, [
        CORE.a({href: '#'}, 'blah')
    ]);

    define(node);
}

require(deps, onReady);

