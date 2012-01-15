var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/nav/index.js', 
    '/ui/core.js', 
    '/donor/index.js',
    '/donor/tom.json'
];

function onReady(E, L, NAV, CORE, DONOR, donordata) {

    function body() {
        return L.spoon([
             L.hug([
                L.pillow(100),
                CORE.box({style: {width: '700px'}}, [
                    DONOR.summary({user: donordata, title: 'Most Influential Donor'}),
                ]),
                CORE.box({style: {width: '300px'}}, [
                    E.h2('Most Popular Charities'),
                    E.ol([
                        E.li([E.a({href: '#'}, 'Global Fund for Women')]),
                        E.li([E.a({href: '#'}, 'Underwater Society of America')])
                    ])
                ]),
            ], 20),

            NAV.footer([
                E.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ], 20);
    }

    return {
        title: 'IContrib.org - Improve the world today.',
        body: body,
        main: NAV.frame([ body() ])
    };
}

return {
    deps: deps,
    callback: onReady
};

