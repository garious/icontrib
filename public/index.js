var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/donor/index.js',
    '/donor/tom.json',
    '/data/userStatus.json'
];

function onReady(E, L, NAV, CORE, DONOR, USER, STATUS) {

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
                            E.li([CORE.a({href: '/donor/?main=GlobalFundForWomen'}, 'Global Fund for Women')]),
                            E.li([CORE.a({href: '/donor/?main=Usoa'}, 'Underwater Society of America')])
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

    return {
        title: 'IContrib.org - Improve the world today',
        body: body,
        main: NAV.frame({userStatus: STATUS}, [ body() ])
    };
}

define(deps, onReady);

