var deps = [
    'tag/tag.js', 
    'nav/index.js', 
    'donor/index.js',
    'donor/tom.json',
];

function onReady(E, NAV, DONOR, donordata) {
    
    function body() {
        return E.div([
            E.div({class: 'container_12 separator'}, [
                DONOR.summary({user: donordata, title: 'Most Influential Donor'}),
                E.div({class: 'grid_4 widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h2(['Most Popular Charities']),
                        E.ol([
                            E.li([E.a({href: '#'}, ['Global Fund for Women'])]),
                            E.li([E.a({href: '#'}, ['Underwater Society of America'])]),
                        ]),
                    ]),
                ]),
            ]),

            E.div({class: 'linkFooter'}, [
                E.a({href: 'charitySignUp/'}, ['Charity Registration'])
            ]),
        ]);
    };

    return {
        title: "IContrib - Improve the world today.",
        body: body,
        main: NAV.frame([
            body(),
        ])
    };
};

return {
    deps: deps,
    callback: onReady
};

