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
        var user = as.user;
	var box = CORE.box([
            L.spoon([
	        E.div({style: {height: '30px'}}, [CORE.h2(user.organizationName)]),
                L.pillow(20),
                L.hug([
                    E.img({style: {width: '175px', height: '175px', borderRadius: '5px'}, src: user.imageUrl, alt: user.organizationName}),
                    L.pillow(30),
                    L.spoon([
                        E.p({style: {height: '100', width: '600'}}, user.mission), 
                        L.pillow(20),
                        CORE.button({href: '/me/?donateTo=' + user.id, text: 'Donate!', loud: true})
                    ])
                ])
            ])
        ]);

        return L.spoon([
           box,
           L.pillow(30)
        ]);
    }

    var main = NAV.frame([charity({user: USER})]);

    define(main);

}

require(deps, onReady);

