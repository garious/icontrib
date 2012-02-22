var authDeps = [
    {path: '/auth/check', interpreter: YOINK.interpreters.json} // TODO: add a '.json' extension to this resource
];

var deps = [
    '/tag/tag.js', 
    '/tag/todom.js', 
    '/tag/layout1.js', 
    'core.js'
];

// TODO: how to get window.innerHeight in IE 8?
function getWindowInnerHeight() {
    return window.innerHeight;
}

function getWindowInnerWidth() {
    return window.innerWidth;
}

function post(path, params, callback) {
    var req = new XMLHttpRequest();
    req.onreadystatechange = function () {
        if (req.readyState === 4) {
            callback(req.responseText);
        }
    };

    var body = JSON.stringify(params);

    req.open('POST', path, true);
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

    req.send(body);
}


function onAuthReady(AUTH) { 
function onReady(E, DOM, L, CORE, ME) { 

    function loginWidget(as) {
        if (AUTH.Left) {
            var username = E.input({type: 'text', size: 18});
            var password = E.input({type: 'password', size: 18});
            var badLogin = E.span({hidden: true, style: {height: 20, width: 200, color: 'red'}}, 'bad username or password');
            var loginButton = CORE.button({text: 'Log in'});
            loginButton.addEventListener('click', function(e) {
                e.preventDefault();
                var formValues = {
                    email: username.value,
                    password: password.value 
                };
                post('/auth/login', formValues, function(dat) {
                    var data = JSON.parse(dat);
                    if(data.Left) {
                        badLogin.hidden = false;
                    } else {
                        window.location.reload();
                    }
                });
            });
            return L.hug([
                L.spoon([
                    L.hug([CORE.label('Username'), username]), L.pillow(5),
                    L.hug([CORE.label('Password'), password]), L.pillow(5),
                    badLogin, L.pillow(5),
                    L.hug([L.pillow(110,0), E.div({style: {width: 90}}, [loginButton])])
                ]), L.pillow(30),
                as.thumbnail
             ]);
        } else {
            var logoutButton = CORE.a({href: '#'}, 'Sign out');
            logoutButton.addEventListener('click', function(e) {
                e.preventDefault();
                post('/auth/logout', {}, function(data) {
                    window.location.reload();
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
            E.img({src: baseUrl + "/logo.png", alt: "IContrib Home", border: "0"})
        ]);

        return E.div({style: {width: getWindowInnerWidth(), height: '129px'}}, [ 
            L.spoon([
                L.pillow(0, 20),
                L.hug([ 
                    L.pillow(250, 0),
                    logo,
                    L.pillow(400, 0),
                    loginWidget(as)
                ])
            ])
        ]);
    }

    //
    // Web Page object
    //
    function webpage(domNode) {
        return {
            constructor: webpage,
            domNode: domNode
        };
    }
    var Page_ToDom = {
        toDom: function (me) {
            return me.domNode;
        },
        getTitle: function (me) {
            return "IContrib.org";
        }
    };
    webpage.interfaces = [
        {'interface': DOM.ToDom, 'instance': Page_ToDom}
    ];

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

        var node = E.div({style: {height: getWindowInnerHeight()}}, [
            L.spoon([
                navbar, 
                L.pillow(20), 
                body
            ])
        ]);

        return webpage(node);
    }

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
 
