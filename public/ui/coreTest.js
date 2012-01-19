var deps = [
    'core.js',
    '/tag/tag.js'
];

function onReady(CORE, E) {

    function row(nm, a, b) {
        return E.tr([
            E.td([E.text(nm)]),
            E.td([a]),
            E.td([b])
        ]);
    }

    function main() {

        return E.table({border: 1, cellpadding: 20}, [
            E.thead([
                E.tr([ E.th('Name'), E.th('Raw'), E.th('Stylized') ])
            ]),
            E.tbody([
                row('h1',     E.h1('E.h1'),              CORE.h1('CORE.h1')),
                row('h2',     E.h2('E.h2'),              CORE.h2('CORE.h2')),
                row('h3',     E.h3('E.h3'),              CORE.h3('CORE.h3')),
                row('h4',     E.h4('E.h4'),              CORE.h4('CORE.h4')),
                row('h5',     E.h5('E.h5'),              CORE.h5('CORE.h5')),
                row('h6',     E.h6('E.h6'),              CORE.h6('CORE.h6')),
                row('p',      E.p('E.p'),                CORE.p('CORE.p')),
                row('a',      E.a({href: '#'}, ['E.a']), CORE.a({href: '#'}, ['CORE.a'])),
                row('label',  E.label('E.label'),        CORE.label('CORE.label')),
                row('button', E.button('E.button'),      CORE.button({href: '#'}, 'CORE.button')),
                row('box',    E.div([E.p(['E.div'])]),   CORE.box())
            ])
        ]);

    }

    return {
       main: main
    };
}

define(deps, onReady);

