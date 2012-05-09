var deps = [
    'Core.js',
    '/Tag/Tag.js'
];

function onReady(Core, Tag) {

    function row(nm, a, b) {
        return Tag.tag('tr', [
            Tag.tag('td', [nm]),
            Tag.tag('td', [a]),
            Tag.tag('td', [b])
        ]);
    }

    function main() {

        return Tag.tag('table', {border: 1, cellpadding: 20}, [
            Tag.tag('thead', [
                Tag.tag('tr', [ Tag.tag('th', 'Name'), Tag.tag('th', 'Raw'), Tag.tag('th', 'Stylized') ])
            ]),
            Tag.tag('tbody', [
                row('h1',     Tag.tag('h1', 'Tag.h1'),              Core.h1('Core.h1')),
                row('h2',     Tag.tag('h2', 'Tag.h2'),              Core.h2('Core.h2')),
                row('h3',     Tag.tag('h3', 'Tag.h3'),              Core.h3('Core.h3')),
                row('h4',     Tag.tag('h4', 'Tag.h4'),              Core.h4('Core.h4')),
                row('h5',     Tag.tag('h5', 'Tag.h5'),              Core.h5('Core.h5')),
                row('h6',     Tag.tag('h6', 'Tag.h6'),              Core.h6('Core.h6')),
                row('p',      Tag.tag('p', 'Tag.p'),                Core.p('Core.p')),
                row('a',      Tag.tag('a', {href: '#'}, ['Tag.a']), Core.hyperlink({url: '#', text: 'Core.hyperlink'})),
                row('label',  Tag.tag('label', 'Tag.label'),        Core.label('Core.label')),
                row('button', Tag.tag('button', 'Tag.button'),      Core.button({href: '#', text: 'Core.button'})),
                row('box',    Tag.tag('div', [Tag.tag('p', ['Tag.div'])]), Core.box({contents: Core.p('Core.box')}))
            ])
        ]);

    }

    Yoink.define(main());
}

Yoink.require(deps, onReady);

