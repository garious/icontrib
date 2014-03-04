var deps = [
    'tag.js'
];

var modules = [
    'interface',
    'interface_test',
    'layout',
    'layout_test',
    'observable',
    'observable_test',
    'shapes',
    'shapes_test'
];

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

