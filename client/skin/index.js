var deps = [
    '/yoink/tag.js',
    '/yoink/layout.js',
    'core.js'
];

var modules = ['chart', 'colors', 'core', 'core-example', 'donor', 'frame', 'frame-example', 'slider'];

function onReady (tag, layout, core) {

    function mkRow (nm) {
        return core.hyperlink({text: nm, url: nm});
    }
    
    var rows = [
        core.h3('Modules')
    ];

    var body = tag.tag({
        name: 'div',
        style: {margin: '10px'},
        contents: [
            layout.vcat(rows.concat(modules.map(mkRow)))

        ]
    });

    yoink.define(body);
}

yoink.require(deps, onReady);

