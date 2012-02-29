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

        var sep = L.pillow(20);

        var listItems = [
            CORE.h2('Recommended Funds'),
            L.pillow(0, 10)
        ];
        for (var i = 0; i < POP.length; i += 1) {
            var x = POP[i];
            listItems.push( CORE.a({href: '/charity/?id=' + x.cid}, x.name) );
        }

        var body = L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({width: '600px'}, [
                    DONOR.profile({user: USER})
                ]),
                sep,
                CORE.box([
                    L.spoon(listItems)
                ])
            ]),
            sep,
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

