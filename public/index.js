var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/donor/index.js',
    '/donor/tom.json'
];

function onReady(E, L, NAV, CORE, DONOR, USER) {

    function body() {
        return L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({style: {width: '600px'}}, [
                    DONOR.summary({user: USER, title: 'Most Influential Donor'})
                ]),
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
            ], 20),
            NAV.footer([
                CORE.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ], 20);
    }

    define({
        title: 'IContrib.org',
        body: body,
        main: NAV.frame([ body() ])
    });
}

require(deps, onReady);

