var deps = [
    'nav.js',
    'core.js',
    '/tag/tag.js'
];

function onReady(NAV, CORE, E) {

    function footer() {
  
        return NAV.footer({style: {backgroundColor: '#EEE'}}, [
            CORE.a({href: '#'}, 'blah')
        ]);

    }

    return {
       main: footer,
       footer: footer
    };
}

define(deps, onReady);

