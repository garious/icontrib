var deps = [
    'tag.js'
];

var modules = ['interface', 'layout', 'layout_test', 'observable', 'shapes', 'shapes_test', 'to-dom'];

function onReady (tag) {

    function mkRow (nm) {
        return tag.tag({
            name: 'a',
            attributes: {href: nm},
            style: {display: 'block'},
            contents: nm
        });
    }
    
    var rows = [
        tag.tag({name: 'h3', contents: 'Modules'})
    ];

    var body = tag.tag({
        name: 'div',
        style: {margin: '10px'},
        contents: rows.concat(modules.map(mkRow))
    });

    yoink.define(body);
}

yoink.require(deps, onReady);

