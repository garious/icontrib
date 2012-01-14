var deps = [
    '/tag/tag.js', 
    '/nav/index.js', 
    '/ui/core.js', 
    '/donor/index.js',
    '/donor/tom.json',
];

function onReady(E, NAV, CORE, DONOR, donordata) {

    function body() {
        return E.div([
            E.div({'class': 'container_12'}, [
                DONOR.summary({user: donordata, title: 'Most Influential Donor'}),
                E.div({'class': 'grid_4'}, [
                    CORE.box([
                        E.h2('Most Popular Charities'),
                        E.ol([
                            E.li([E.a({href: '#'}, 'Global Fund for Women')]),
                            E.li([E.a({href: '#'}, 'Underwater Society of America')])
                        ])
                    ])
                ])
            ]),

            NAV.footer([
                E.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ]);
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

