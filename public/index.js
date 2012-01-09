var deps = [
    'tag/tag.js', 
    'nav/index.js', 
    'donor/eric.js',
];

function onReady(E, NAV, DONOR) {
    
    function body() {
        return E.div([
            E.div({class: 'container_12'}, [
                E.div({id: 'call-to-action', class: 'grid_12'}, [
                    E.div({class: 'widgetContent'}, [
                        E.a({href: 'signup/'}, ['Get started!']),
                        'Please excuse our mess.  icontrib.org is under construction.  The people, the organizations, everything - not real.'
                    ]),
                ]),
            ]),

            E.div({class: 'container_12 separator'}, [
                DONOR.body(),
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
                E.a({href: 'charitySignUp'}, ['Charity Sign Up'])
            ]),
        ]);
    };

    return {
        title: "IContrib - Improve the world today.",
        body: body,
        main: NAV.frame([
            body(),
        ]),
    };
};

return {
    deps: deps,
    callback: onReady,
};

