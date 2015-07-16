var dom = require('poochie/dom');
var layout = require('poochie/layout');
var core = require('./core');

var modules = ['chart', 'colors', 'core', 'core-example', 'donor', 'frame', 'frame-example', 'slider'];

function mkRow (nm) {
    return core.hyperlink({text: nm, url: nm});
}

var rows = [
    core.h3('Modules')
];

module.exports = dom.element({
    name: 'div',
    style: {margin: '10px'},
    contents: [
        layout.vcat(rows.concat(modules.map(mkRow)))
    ]
};

