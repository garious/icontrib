var deps = [
    '/stdlib/dom.js',
    '/stdlib/layout.js',
    'core.js'
];

var modules = ['chart', 'colors', 'core', 'core-example', 'donor', 'frame', 'frame-example', 'slider'];

function onReady (dom, layout, core) {

    function mkRow (nm) {
        return core.hyperlink({text: nm, url: nm});
    }
    
    var rows = [
        core.h3('Modules')
    ];

    var body = dom.element({
        name: 'div',
        style: {margin: '10px'},
        contents: [
            layout.vcat(rows.concat(modules.map(mkRow)))

        ]
    });

    define(body);
}

require(deps, onReady);

