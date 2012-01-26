var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/core.js', 
    '/ui/nav.js'
];

function onReady(E, L, CORE, NAV) {
    
    function charity(as) {
        as = as || {};
        var user = as.user || defaultUser;
	var box = CORE.box([
            L.spoon([
	        CORE.h2(user.firstName),
                L.hug([
                    L.spoon([
                        E.img({style: {width: '175px', height: '175px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName})
                    ], 20),
                    L.spoon([
                        E.p({style: {width: '600'}}, user.mission),
                        E.br(),
                        CORE.button({href: '/me/?donateTo=' + user.id}, ['Donate!'])
                    ], 20)
                ], 30)
            ], 20)
        ]);

        return L.spoon([
           L.hug([
                L.pillow(200,0),
                box
           ], 20),
           L.pillow(30)
        ]);
    }

    function Usoa(params, nodeReady) {
        require(['usoa.json'], function(u) {
            nodeReady( NAV.frame([charity({user: u})]) );
        });
    }

    function GlobalFundForWomen(params, nodeReady) {
        require(['gffw.json'], function(u) {
            nodeReady( NAV.frame([charity({user: u})]) );
        });
    }

    define({
        title: 'IContrib.org',
        main: Usoa,
        usoa: Usoa,
        gffw: GlobalFundForWomen
    });

}

require(deps, onReady);

