var authDeps = [
    {path: '/auth/check', interpreter: YOINK.interpreters.json} // TODO: add a '.json' extension to this resource
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    'core.js',
    '/jquery/jquery-mod.js'
];

// TODO: how to get window.innerHeight in IE 8?
function getWindowInnerHeight() {
    return window.innerHeight;
}

function getWindowInnerWidth() {
    return window.innerWidth;
}

function onAuthReady(AUTH) { 
function onReady(E, L, CORE, $, ME) { 

    function loginWidget(as) {
        if (AUTH.Left) {
            var username = E.input({type: 'text', size: 18});
            var password = E.input({type: 'password', size: 18});
            var badLogin = E.span({hidden: true, style: {height: 20, width: 200, color: 'red'}}, 'bad username or password');
            var loginButton = CORE.button('Log in');
            $(loginButton).click(function(e) {
                e.preventDefault();
                var formValues = {
                    UserLogin: {
                        email: username.value,
                        password: password.value 
                    }
                };
                $.ajax({
                    type: 'POST',
                    url: '/auth/login',
                    data: JSON.stringify(formValues),
                    dataType: 'json',
                    success: function(data) {
                        if(data.Left) {
                            badLogin.hidden = false;
                        } else {
                            window.location.reload();
                        }
                    }
                });
            });
            return L.hug([
                L.spoon([
                    L.hug([CORE.label('Username'), username]), L.pillow(5),
                    L.hug([CORE.label('Password'), password]), L.pillow(5),
                    badLogin, L.pillow(5),
                    loginButton
                ]), L.pillow(30),
                as.thumbnail
             ]);
        } else {
            var logoutButton = CORE.a({href: '#'}, 'Sign out');
            $(logoutButton).click(function(e) {
                e.preventDefault();
                $.ajax({
                    type: 'GET',
                    url: '/auth/logout',
                    success: function(data) {
                        window.location.reload();
                    }
                });
            });

           return L.hug([
               as.thumbnail,
               L.pillow(30),
               E.div({style: {width: 100}}, [logoutButton])
           ]);
        }
    }

    function nav(as) {
        as = as || {};

        var logo = E.a({href: '/', style: {width: '129px'}}, [
            E.img({src: "/images/logo.png", alt: "IContrib Home", border: "0"})
        ]);

        return E.div({style: {width: getWindowInnerWidth(), height: '129px'}}, [ 
            L.hug([ 
                L.pillow(250, 0),
                logo,
                L.pillow(25, 0),
                L.spoon([
                    L.pillow(450, 40),
                    E.div({style: {color: '#D0D0D0'}}, [
                        CORE.h1('Improve the world today')
                    ])
                ]),
                L.spoon([
                    L.pillow(0, 10),
                    loginWidget(as),
                ]),
            ])
        ]);
    }

    function dockItem(as) {
        as = as || {};
        var e = E.a({href: as.href}, [ E.img({src: as.src, alt: as.title, title: as.title}) ]);
        return e;
    };

    function dock() {
        //var as = {align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300};
        var xs = [
            dockItem({href: '/',         src: '/images/home.png', title: "Home"}),
            dockItem({href: '/donor/',   src: '/images/portfolio.png', title: "Your Portfolio"}),
            dockItem({href: '/charity/', src: '/images/link.png', title: "Charities"}),
            dockItem({href: '/contact/', src: '/images/rss.png', title: "Keep Informed"})
        ];
        return E.div({'class': 'footer'}, [E.div({'class': 'navBar'}, xs)]);
    };

    function frame(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};

        var imageUrl = AUTH.Left && '/donor/anonymous.jpg' || ME.imageUrl;

        var thumbnail = E.a({href: '/me/', style: {width: '110px', height: '110px'}}, [
            E.img({style: {width: '110px', height: '110px', borderRadius: '5px'}, src: imageUrl, alt: ME.firstName + ' ' + ME.lastName})
        ]);

        as.thumbnail = thumbnail;

        var navbar = nav(as);
        var body = E.div(xs);

        var e = E.div({style: {height: getWindowInnerHeight()}}, [L.spoon([navbar, L.pillow(20), body])]);
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
        as.style.width = getWindowInnerWidth();
        as.style.textAlign = 'center';

        return E.div(as, [
            E.hr(),
            E.div({style: {paddingRight: '20px'}}, xs),
            E.hr()
        ]); 
    }

    function userInfo() {
        return ME;
    }

    define({
        nav: nav,
        dock: dock,
        dockItem: dockItem,
        frame: frame,
        footer: footer,
        userInfo: userInfo
    });

}

var donorId = AUTH.Left && 'anonymous' || AUTH.Right;
var donorUrl = '/donor/' + donorId + '.json';
deps.push(donorUrl);
require(deps, onReady);
}

require(authDeps, onAuthReady);
 
