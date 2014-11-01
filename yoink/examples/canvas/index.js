var deps = [
    '/stdlib/dom.js',
];

var modules = ['canvas-example', 'canvas-pie-example'];

function onReady (dom) {

    function mkRow (nm) {
        return dom.element({
            name: 'a',
            attributes: {href: nm},
            style: {display: 'block'},
            contents: nm
        });
    }
    
    var rows = [
        dom.element({name: 'h3', contents: 'Modules'})
    ];

    var body = dom.element({
        name: 'div',
        style: {margin: '10px'},
        contents: rows.concat(modules.map(mkRow))
    });

    define(body);
}

require(deps, onReady);

