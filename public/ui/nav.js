var authDeps = [
    '/auth/check.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/todom.js', 
    '/tag/layout1.js', 
    'core.js',
    'colors.js'
];

// TODO: Find this function a better home.
function getDimensions(me) {
    return {
        width: parseInt(me.style.width, 10),
        height: parseInt(me.style.height, 10)
    };
}


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
function onReady(E, DOM, L, CORE, COLOR, ME) { 

    function loginWidget(as) {
        var username = E.input({type: 'text', size: 18});
        var password = E.input({type: 'password', size: 18});

        function submit(e) {
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
                    window.location = '/me/';
                }
            });
        }

        if (AUTH.Left) {
            var badLogin = E.span({hidden: true, style: {height: 20, width: 200, color: 'red'}}, 'bad username or password');
            var loginButton = CORE.button({text: 'Log in'});

            loginButton.addEventListener('click', submit);

            var widget = L.hug([
                L.spoon([
                    L.hug([CORE.label('Username'), username]), L.pillow(5),
                    L.hug([CORE.label('Password'), password]), L.pillow(5),
                    badLogin,
                    L.pillow(5)
                ]),
                loginButton
            ]);

            widget.addEventListener('keyup', function(e) {
                if (e.keyCode === 13) {
                   submit(e);
                }
            });

            return widget;

        } else {
            //var logoutButton = CORE.a({href: '#'}, 'Sign out');
            var logoutButton = E.img({src: baseUrl + '/arrowdown-darkgreen.png', alt: 'settings'});
            logoutButton.addEventListener('click', function(e) {
                e.preventDefault();
                post('/auth/logout', {}, function(data) {
                    window.location = '/';
                });
            });

           return L.hug([
               as.thumbnail,
               L.pillow(50, 0),  // TODO: This should be '20', not '50', but there's a bug in calculating the size of the thumbnail
               L.spoon([
                   L.pillow(0, 22),
                   logoutButton
               ])
           ]);
        }
    }

    function nav(as) {
        as = as || {};

        var logo = E.a({href: '/', style: {width: '129px', height: '70px'}}, [
            E.img({src: baseUrl + "/logo.png", alt: "IContrib Home", border: "0"})
        ]);

        return L.spoon([
            L.pillow(0, 20),
            L.hug([
                logo,
                L.pillow(580, 0),
                loginWidget(as)
            ]),
            E.hr({style: {width: '960px', height: '4px', backgroundColor: COLOR.green, borderWidth: 1}})
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

        if (AUTH.Right) {
            var thumbContents = L.hug([
                E.img({style: {width: '50px', height: '50px'}, src: ME.imageUrl, alt: ME.firstName + ' ' + ME.lastName}),
                L.pillow(20, 0),
                L.spoon([
                    L.pillow(0, 10),
                    CORE.h3([ME.firstName + ' ' + ME.lastName])
                ])
            ]);

            var dim = getDimensions(thumbContents);

            var thumbnail = CORE.a({href: '/me/', style: {width: dim.width, height: dim.height}}, [
                thumbContents
            ]);

            as.thumbnail = thumbnail;
        }

        var navbar = nav(as);
        var body = E.div(xs);

        var node = E.div({style: {margin: '0px auto', height: getWindowInnerHeight(), width: '960px'}}, [
            L.spoon([
                navbar, 
                L.pillow(50), 
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
 
