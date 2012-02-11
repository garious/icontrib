var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/core.js', 
    '/ui/nav.js',
    params.id + '.json'
];

function onReady(E, L, CORE, NAV, USER) {
    
    function charity(as) {
        as = as || {};
        var user = as.user || defaultUser;
	var box = CORE.box([
            L.spoon([
	        E.div({style: {height: '30px'}}, [CORE.h2(user.firstName)]),
                L.pillow(20),
                L.hug([
                    E.img({style: {width: '175px', height: '175px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName}),
                    L.pillow(30),
                    L.spoon([
                        E.p({style: {height: '100', width: '600'}}, user.mission), 
                        L.pillow(20),
                        CORE.button({href: '/me/?donateTo=' + user.id}, ['Donate!'])
                    ])
                ])
            ])
        ]);

        return L.spoon([
           L.hug([
                L.pillow(220,0),
                box
           ]),
           L.pillow(30)
        ]);
    }

    define( NAV.frame([charity({user: USER})]) );

}

require(deps, onReady);

