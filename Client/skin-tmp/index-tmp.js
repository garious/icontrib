var deps = [
    '/yoink/tag.js',
    '/yoink/layout.js',
    'Core.js'
];

var modules = ['Chart', 'Colors', 'Core', 'CoreTest', 'Donor', 'Frame', 'FrameTest', 'Slider'];

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
            layout.spoon(rows.concat(modules.map(mkRow)))

        ]
    });

    yoink.define(body);
}

yoink.require(deps, onReady);

