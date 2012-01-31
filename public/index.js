var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/donor/index.js'
];

function onInitialReady(BEST) {

function onReady(E, L, NAV, CORE, DONOR, USER) {

    function body() {
        return L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({style: {width: '600px'}}, [
                    DONOR.summary({user: USER, title: 'Most Influential Donor'})
                ]),
                L.pillow(20),
                CORE.box({style: {width: '350px', height: '120px'}}, [
                    L.spoon([
                        CORE.h2('Most Popular Charities'),
                        E.ol({style: {width: '350px'}}, [
                            E.li([CORE.a({href: '/charity/?main=gffw'}, 'Global Fund for Women')]),
                            E.li([CORE.a({href: '/charity/?main=usoa'}, 'Underwater Society of America')])
                        ]),
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

