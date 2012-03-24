var deps = [
    'core.js',
    '/tag/tag.js'
];

function onReady(Core, Tag) {

    function row(nm, a, b) {
        return Tag.tr([
            Tag.td([Tag.text(nm)]),
            Tag.td([a]),
            Tag.td([b])
        ]);
    }

    function main() {

        return Tag.table({border: 1, cellpadding: 20}, [
            Tag.thead([
                Tag.tr([ Tag.th('Name'), Tag.th('Raw'), Tag.th('Stylized') ])
            ]),
            Tag.tbody([
                row('h1',     Tag.h1('Tag.h1'),              Core.h1('Core.h1')),
                row('h2',     Tag.h2('Tag.h2'),              Core.h2('Core.h2')),
                row('h3',     Tag.h3('Tag.h3'),              Core.h3('Core.h3')),
                row('h4',     Tag.h4('Tag.h4'),              Core.h4('Core.h4')),
                row('h5',     Tag.h5('Tag.h5'),              Core.h5('Core.h5')),
                row('h6',     Tag.h6('Tag.h6'),              Core.h6('Core.h6')),
                row('p',      Tag.p('Tag.p'),                Core.p('Core.p')),
                row('a',      Tag.a({href: '#'}, ['Tag.a']), Core.hyperlink({url: '#', text: 'Core.hyperlink'})),
                row('label',  Tag.label('Tag.label'),        Core.label('Core.label')),
                row('button', Tag.button('Tag.button'),      Core.button({href: '#', text: 'Core.button'})),
                row('box',    Tag.div([Tag.p(['Tag.div'])]), Core.box({contents: Core.p('Core.box')}))
            ])
        ]);

    }

    define(main());
}

require(deps, onReady);

