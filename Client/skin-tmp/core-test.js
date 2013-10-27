var deps = [
    'Core.js',
    '/yoink/tag.js'
];

function onReady(core, tag) {

    function row(nm, a, b) {
        return tag.tag({name: 'tr', contents: [
            tag.tag({name: 'td', contents: [nm]}),
            tag.tag({name: 'td', contents: [a]}),
            tag.tag({name: 'td', contents: [b]})
        ]});
    }

    function main() {

        return tag.tag({
            name: 'table',
            attributes: {border: 1, cellpadding: 20},
            contents: [
                //tag.tag('thead', [
                    tag.tag('tr', [ tag.tag('th', 'Name'), tag.tag('th', 'Raw'), tag.tag('th', 'Stylized') ]),
                //]),
                //tag.tag('tbody', [
                    row('h1',     tag.tag({name: 'h1', contents: 'tag.h1'}),              core.h1('core.h1')),
                    row('h2',     tag.tag({name: 'h2', contents: 'tag.h2'}),              core.h2('core.h2')),
                    row('h3',     tag.tag({name: 'h3', contents: 'tag.h3'}),              core.h3('core.h3')),
                    row('h4',     tag.tag({name: 'h4', contents: 'tag.h4'}),              core.h4('core.h4')),
                    row('h5',     tag.tag({name: 'h5', contents: 'tag.h5'}),              core.h5('core.h5')),
                    row('h6',     tag.tag({name: 'h6', contents: 'tag.h6'}),              core.h6('core.h6')),
                    row('p',      tag.tag({name: 'p', contents: 'tag.p'}),                core.p('core.p')),
                    row('a',      tag.tag({name: 'a', attributes: {href: '#'}, contents: ['tag.a']}), core.hyperlink({url: '#', text: 'core.hyperlink'})),
                    row('label',  tag.tag({name: 'label', contents: 'tag.label'}),        core.label('core.label')),
                    row('button', tag.tag({name: 'button', contents: 'tag.button'}),      core.button({href: '#', text: 'core.button'})),
                    row('box',    tag.tag({name: 'div', contents: [tag.tag({name: 'p', contents: ['tag.div']})]}), core.box({contents: core.p('core.box')}))
                //])
            ]
        });

    }

    yoink.define(main());
}

yoink.require(deps, onReady);

