var deps = [
    'Core.js',
    '/Tag/Tag.js'
];

function onReady(Core, Tag) {

    function row(nm, a, b) {
        return Tag.tag({name: 'tr', contents: [
            Tag.tag({name: 'td', contents: [nm]}),
            Tag.tag({name: 'td', contents: [a]}),
            Tag.tag({name: 'td', contents: [b]})
        ]});
    }

    function main() {

        return Tag.tag({
            name: 'table',
            attributes: {border: 1, cellpadding: 20},
            contents: [
                Tag.tag('thead', [
                    Tag.tag('tr', [ Tag.tag('th', 'Name'), Tag.tag('th', 'Raw'), Tag.tag('th', 'Stylized') ])
                ]),
                Tag.tag('tbody', [
                    row('h1',     Tag.tag({name: 'h1', contents: 'Tag.h1'}),              Core.h1('Core.h1')),
                    row('h2',     Tag.tag({name: 'h2', contents: 'Tag.h2'}),              Core.h2('Core.h2')),
                    row('h3',     Tag.tag({name: 'h3', contents: 'Tag.h3'}),              Core.h3('Core.h3')),
                    row('h4',     Tag.tag({name: 'h4', contents: 'Tag.h4'}),              Core.h4('Core.h4')),
                    row('h5',     Tag.tag({name: 'h5', contents: 'Tag.h5'}),              Core.h5('Core.h5')),
                    row('h6',     Tag.tag({name: 'h6', contents: 'Tag.h6'}),              Core.h6('Core.h6')),
                    row('p',      Tag.tag({name: 'p', contents: 'Tag.p'}),                Core.p('Core.p')),
                    row('a',      Tag.tag({name: 'a', attributes: {href: '#'}, contents: ['Tag.a']}), Core.hyperlink({url: '#', text: 'Core.hyperlink'})),
                    row('label',  Tag.tag({name: 'label', contents: 'Tag.label'}),        Core.label('Core.label')),
                    row('button', Tag.tag({name: 'button', contents: 'Tag.button'}),      Core.button({href: '#', text: 'Core.button'})),
                    row('box',    Tag.tag({name: 'div', contents: [Tag.tag({name: 'p', contents: ['Tag.div']})]}), Core.box({contents: Core.p('Core.box')}))
                ])
            ]
        });

    }

    Yoink.define(main());
}

Yoink.require(deps, onReady);

