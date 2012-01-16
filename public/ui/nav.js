var deps = [
    '../tag/tag.js', 
    '../tag/layout.js', 
    'colors.json', 
    'core.js',
    'login.js',
    '../jquery/jquery-mod.js'
];

function onReady(E, L, C, CORE, LOGIN, $) { 

    function nav(as) {
        as = as || {};

        var headerStyle = {
            overflow: 'auto', // Required for IE
            top: 0,
            left: 0,
            width: '100%',
            height: '80px',
            backgroundColor: C.midDarkColor
        };
        
        var taglineStyle = {
            fontSize: '1.75em',
            color: C.accentColor,
            position: 'absolute',
            left: '125px',
            top: '20px'
        };

        var loginStyle = {
            position: 'absolute',
            right: '25px',
            top: '15px'
        };

        //var errorBox = E.div();

        //var loginForm = E.form({style: {marginBottom: 0}}, [
        //    CORE.box([
        //      L.hug([
        //        CORE.label("Email "),    E.input({type: "text", name: "email", size: "20"}),
        //        L.pillow(20),
        //        CORE.label("Password "), E.input({type: "password", name: "password", size: "10"}),
        //        L.pillow(20),
        //        E.input({type: 'submit', value: 'Log in'}),
        //        errorBox,
        //        L.pillow(20)
        //      ])
        //    ])
        //]);

        var loginForm;
        if (as.userStatus && as.userStatus.Right) {
            loginForm = E.text(as.userStatus.Right);
        } else {
            loginForm = LOGIN.loginForm('/login_user');
        }

        return E.div({style: headerStyle}, [ 
            E.a({href: '/'}, [
                E.img({src: "/images/logo4.png", alt: "IContrib Home", style: {height: "100%"}, border: "0"})
            ]),

            E.div({style: taglineStyle}, [
                CORE.h2('Improve the world today')
            ]),

            CORE.box({style: loginStyle}, [loginForm])
        ]);
    };

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
        e.style.backgroundColor = '#EEE';
        return e;
    };


    var footerStyle = {
        bottom: '0px',
        width: '100%',
        height: '23px',
        color: C.accentColor,
        textAlign: 'right'
    };

    function footer(xs) {
        return E.div({style: footerStyle}, [
            E.hr(),
            E.div({style: {paddingRight: '20px'}}, xs),
            E.hr()
        ]); 
    }

    return {
        nav: nav,
        dock: dock,
        dockItem: dockItem,
        frame: frame,
        footer: footer
    };

}

define(deps, onReady);
 
