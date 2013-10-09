var deps = [
    'Tag.js'
];

var modules = ['Interface', 'Layout', 'LayoutTest', 'Observable', 'Shapes', 'ShapesTest', 'ToDom', 'Webpage'];

function onReady (Tag) {

    function mkRow (nm) {
        return Tag.tag({
            name: 'a',
            attributes: {href: nm},
            style: {display: 'block'},
            contents: nm
        });
    }
    
    var rows = [
        Tag.tag({name: 'h3', contents: 'Modules'})
    ];

    var body = Tag.tag({
        name: 'div',
        style: {margin: '10px'},
        contents: rows.concat(modules.map(mkRow))
    });

    Yoink.define(body);
}

Yoink.require(deps, onReady);

