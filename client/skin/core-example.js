var core = require('./core');
var dom = require('poochie/dom');

function row(nm, a, b) {
    return dom.element({name: 'tr', contents: [
        dom.element({name: 'td', contents: [nm]}),
        dom.element({name: 'td', contents: [a]}),
        dom.element({name: 'td', contents: [b]})
    ]});
}

function main() {

    return dom.element({
        name: 'table',
        attributes: {border: 1, cellpadding: 20},
        contents: [
            //dom.element('thead', [
                dom.element('tr', [ dom.element('th', 'Name'), dom.element('th', 'Raw'), dom.element('th', 'Stylized') ]),
            //]),
            //dom.element('tbody', [
                row('h1',     dom.element({name: 'h1', contents: 'dom.h1'}),              core.h1('core.h1')),
                row('h2',     dom.element({name: 'h2', contents: 'dom.h2'}),              core.h2('core.h2')),
                row('h3',     dom.element({name: 'h3', contents: 'dom.h3'}),              core.h3('core.h3')),
                row('h4',     dom.element({name: 'h4', contents: 'dom.h4'}),              core.h4('core.h4')),
                row('h5',     dom.element({name: 'h5', contents: 'dom.h5'}),              core.h5('core.h5')),
                row('h6',     dom.element({name: 'h6', contents: 'dom.h6'}),              core.h6('core.h6')),
                row('p',      dom.element({name: 'p', contents: 'dom.p'}),                core.p('core.p')),
                row('a',      dom.element({name: 'a', attributes: {href: '#'}, contents: ['dom.a']}), core.hyperlink({url: '#', text: 'core.hyperlink'})),
                row('label',  dom.element({name: 'label', contents: 'dom.label'}),        core.label('core.label')),
                row('button', dom.element({name: 'button', contents: 'dom.button'}),      core.button({href: '#', text: 'core.button'})),
                row('box',    dom.element({name: 'div', contents: [dom.element({name: 'p', contents: ['dom.div']})]}), core.box({contents: core.p('core.box')}))
            //])
        ]
    });

}

module.exports = main();

