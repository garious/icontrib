var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    'colors.json', 
    'core.js',
    'login.js',
    '/jquery/jquery-mod.js'
];

function onReady(E, L, C, CORE, LOGIN, $) { 

    function nav(as) {
        as = as || {};

        var headerStyle = {
            position: 'absolute',
            overflow: 'auto', // Required for IE
            top: 0,
            left: 0,
            width: '100%',
            height: '129px'
        };

        var imgStyle = {
            position: 'absolute',
            top: 0,
            left: '250px',
            height: '100%'
        };
        
        var taglineStyle = {
            color: '#D0D0D0',
            position: 'absolute',
            left: '425px',
            top: '50px'
        };

        var loginStyle = {
            position: 'absolute',
            left: '850px',
            top: '10px'
        };

        var loginForm = LOGIN.loginForm({ login: '/user/login', logout: '/user/logout', check: '/user/check'});

        return E.div({style: headerStyle}, [ 
            E.a({href: '/'}, [
                E.img({src: "/images/logo.png", alt: "IContrib Home", style: imgStyle, border: "0"})
            ]),

            E.div({style: taglineStyle}, [
                CORE.h1('Improve the world today')
            ]),

            E.div({style: loginStyle}, [loginForm])
        ]);
    }

    var dockItem = function(as) {
        as = as || {};
        var e = E.a({href: as.href}, [ E.img({src: as.src, alt: as.title, title: as.title}) ]);
        return e;
    };

    var dock = function(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        return E.div({'class': 'footer'}, [E.div({'class': 'navBar'}, xs)]);
    };

    var frame = function(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};
        
        var navbar = nav(as);

        var doc = dock( /*{align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300},*/ [
            dockItem({href: '/',         src: '/images/home.png', title: "Home"}),
            dockItem({href: '/donor/',   src: '/images/portfolio.png', title: "Your Portfolio"}),
            dockItem({href: '/charity/', src: '/images/link.png', title: "Charities"}),
            dockItem({href: '/contact/', src: '/images/rss.png', title: "Keep Informed"})
        ]);

        var body = E.div(xs);

        var e = L.spoon([navbar, body/*, doc*/], 20);
        e.style.backgroundColor = '#F3F3F6';
        return e;
    };

    function footer(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        as.style = as.style || {};
        as.style.bottom = '0px';
        as.style.width = '100%';
        as.style.textAlign = 'center';

        return E.div(as, [
            E.hr(),
            E.div({style: {paddingRight: '20px'}}, xs),
            E.hr()
        ]); 
    }

    define({
        nav: nav,
        dock: dock,
        dockItem: dockItem,
        frame: frame,
        footer: footer
    });

}

require(deps, onReady);
 
