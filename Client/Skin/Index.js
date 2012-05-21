var deps = [
    '/Tag/Tag.js',
    '/Tag/Layout.js',
    'Core.js'
];

var modules = ['Chart', 'Colors', 'Core', 'CoreTest', 'Donor', 'Frame', 'FrameTest', 'Slider'];

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

