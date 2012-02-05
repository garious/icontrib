var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/charity/popular.json', 
    '/donor/index.js'
];

function onInitialReady(BEST) {

function onReady(E, L, NAV, CORE, POP, DONOR, USER) {

    function body() {

        var listItems = [];
        for (var i = 0; i < POP.length; i += 1) {
            var x = POP[i];
            listItems.push( E.li([CORE.a({href: '/charity/?id=' + x.id}, x.name)]) );
        }

        return L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({style: {width: '600px'}}, [
                    DONOR.profile({user: USER})
                ]),
                L.pillow(20),
                CORE.box({style: {width: '350px', height: '120px'}}, [
                    L.spoon([
                        CORE.h2('Recommended Funds'),
                        E.ol({style: {width: '350px'}}, listItems),
                        L.pillow(30)
                    ])
                ])
            ]),
            NAV.footer([
                CORE.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ], 20);
    }

    function main() {
        return NAV.frame([ body() ]);
    }

    define({
        title: 'IContrib.org',
        body: body,
        main: main
    });
}

deps.push('/donor/' + BEST + '.json');
require(deps, onReady);
}

require(initialDeps, onInitialReady);

