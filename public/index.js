var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/charity/popular.json', 
    '/donor/index.js'
];

function onInitialReady(BEST) {

    function onReady(E, L, NAV, CORE, POP, DONOR, USER) {

        var listItems = [
            CORE.h2('Recommended Funds')
        ];
        for (var i = 0; i < POP.length; i += 1) {
            var x = POP[i];
            listItems.push( CORE.a({href: '/charity/?id=' + x.id}, x.name) );
        }

        var body = L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({width: '600px'}, [
                    DONOR.profile({user: USER})
                ]),
                L.pillow(20),
                CORE.box([
                    L.spoon(listItems)
                ])
            ]),
            NAV.footer([
                CORE.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ]);

        define( NAV.frame([body]) );

    }

    deps.push('/donor/' + BEST + '.json');
    require(deps, onReady);
}

require(initialDeps, onInitialReady);

