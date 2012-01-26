var authDeps = [
    {path: '/auth/check', interpreter: YOINK.interpreters.json} // TODO: add a '.json' extension to this resource
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    'core.js',
    '/jquery/jquery-mod.js'
];

function onAuthReady(AUTH) { 
function onReady(E, L, CORE, $, ME) { 

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
            left: '875px',
            top: '10px'
        };

        var logo = E.a({href: '/'}, [
            E.img({src: "/images/logo.png", alt: "IContrib Home", style: imgStyle, border: "0"})
        ]);

        var loginWidget;
        if (AUTH.Left) {
            var username = E.input({type: 'text'});
            var password = E.input({type: 'password'});
            var badLogin = E.span({hidden: true, style: {height: 20, color: 'red'}}, 'bad username or password');
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
            loginWidget = L.hug({style: loginStyle}, [
                    L.spoon([
                        L.hug([CORE.label('Username'), username]),
                        L.hug([CORE.label('Password'), password]),
                        badLogin,
                        loginButton
                    ], 5),
                    as.thumbnail
                ], 30);
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

           loginWidget = L.hug({style: loginStyle}, [
              as.thumbnail,
              E.div({style: {width: 100}}, [logoutButton])
           ], 30);
        }

        return L.spoon([ 
            E.div({style: headerStyle}, [ 
                logo,
                E.div({style: taglineStyle}, [
                    CORE.h1('Improve the world today')
                ]),
                loginWidget
            ])
        ]);
    }

    var dockItem = function(as) {
        as = as || {};
        var e = E.a({href: as.href}, [ E.img({src: as.src, alt: as.title, title: as.title}) ]);
        return e;
    };

    var dock = function() {
        //var as = {align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300};
        var xs = [
            dockItem({href: '/',         src: '/images/home.png', title: "Home"}),
            dockItem({href: '/donor/',   src: '/images/portfolio.png', title: "Your Portfolio"}),
            dockItem({href: '/charity/', src: '/images/link.png', title: "Charities"}),
            dockItem({href: '/contact/', src: '/images/rss.png', title: "Keep Informed"})
        ];
        return E.div({'class': 'footer'}, [E.div({'class': 'navBar'}, xs)]);
    };

    var frame = function(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};

        var imageUrl = AUTH.Left && '/donor/anonymous.jpg' || ME.imageUrl;

        var thumbnail = E.a({href: '/me/'}, [
            E.img({style: {width: '110px', height: '110px', borderRadius: '5px'}, src: imageUrl, alt: ME.firstName + ' ' + ME.lastName})
        ]);

        as.thumbnail = thumbnail;

        var navbar = nav(as);
        var body = E.div(xs);

        var e = E.div({style: {height: '100%'}}, [L.spoon([navbar, body], 20)]);
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
 
