var deps = [
    '/Tag/Tag.js',
    '/Tag/Layout.js',
    '/Skin/Core.js'
];

var modules = ['CanvasExample', 'CanvasPieExample', 'Interface', 'Layout', 'LayoutTest', 'Observable', 'Shapes', 'ShapesTest', 'ToDom', 'TwoDimensional', 'Webpage'];

function onReady (Tag, Layout, Core) {

    function mkRow (nm) {
        return Core.hyperlink({text: nm, url: nm});
    }
    
    var rows = [
        Core.h3('Modules')
    ];

    var body = Tag.tag({
        name: 'div',
        style: {margin: '10px'},
        contents: [
            Layout.spoon(rows.concat(modules.map(mkRow)))

        ]
    });

    Yoink.define(body);
}

Yoink.require(deps, onReady);

