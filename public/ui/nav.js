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


function onAuthReady(Auth) { 
function onReady(Tag, ToDom, Layout, Core, Colors, Me) { 

    function loginWidget(as) {
        var username = Core.input({type: 'text', size: 18});
        var password = Core.input({type: 'password', size: 18});

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

        if (Auth.Left) {
            var badLogin = Tag.span({hidden: true, style: {height: '20px', width: '200px', color: 'red'}}, 'bad username or password');
            var loginButton = Core.button({text: 'Log in'});

            loginButton.addEventListener('click', submit);

            var widget = Layout.hug([
                Layout.spoon([
                    Layout.hug([Core.label('Username'), Layout.pillow(5, 0), username]),
                    Layout.pillow(0, 5),
                    Layout.hug([Core.label('Password'), Layout.pillow(5, 0), password]),
                    Layout.pillow(0, 5),
                    badLogin,
                    Layout.pillow(0, 5)
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
            //var logoutButton = Core.a({href: '#'}, 'Sign out');
            var logoutButton = Tag.img({src: baseUrl + '/arrowdown-darkgreen.png', alt: 'settings'});
            logoutButton.addEventListener('click', function(e) {
                e.preventDefault();
                post('/auth/logout', {}, function(data) {
                    window.location = '/';
                });
            });

           return Tag.div({style: {width: '270px', height: '77px', backgroundColor: '#eee', borderRadius: '5px 5px 0px 0px', border: '1px solid', borderColor: Colors.lightColor}}, [
               Layout.spoon([
                   Layout.pillow(0, 15),
                   Layout.hug([
                       Layout.pillow(20, 0),
                       as.thumbnail,
                       Layout.pillow(50, 0),  // TODO: This should be '20', not '50', but there's a bug in calculating the size of the thumbnail
                       Layout.spoon([
                           Layout.pillow(0, 22),
                           logoutButton
                       ])
                   ])
               ])
           ]);
        }
    }

    function nav(as) {
        as = as || {};

        var logo = Tag.a({href: '/', style: {width: '129px', height: '70px'}}, [
            Tag.img({src: baseUrl + "/logo.png", alt: "IContrib Home", border: "0"})
        ]);

        return Layout.spoon([
            Layout.pillow(0, 20),
            Layout.hug([
                logo,
                Layout.pillow(560, 0),
                loginWidget(as)
            ]),
            Tag.hr({style: {width: '960px', height: '4px', margin: 0, backgroundColor: Colors.green, borderWidth: 1}})
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
        {'interface': ToDom.ToDom, 'instance': Page_ToDom}
    ];

    function frame(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};

        if (Auth.Right) {
            var thumbContents = Layout.hug([
                Tag.img({style: {width: '50px', height: '50px'}, src: Me.imageUrl, alt: Me.firstName + ' ' + Me.lastName}),
                Layout.pillow(20, 0),
                Layout.spoon([
                    Layout.pillow(0, 10),
                    Core.h3({
                        color: Colors.greenText,
                        text: Me.firstName + ' ' + Me.lastName
                    })
                ])
            ]);

            var dim = getDimensions(thumbContents);

            var thumbnail = Core.a({href: '/me/', style: {width: dim.width, height: dim.height}}, [
                thumbContents
            ]);

            as.thumbnail = thumbnail;
        }

        var navbar = nav(as);
        var body = Tag.div(xs);

        var node = Tag.div({style: {margin: '0px auto', height: getWindowInnerHeight(), width: '960px'}}, [
            Layout.spoon([
                navbar, 
                Layout.pillow(50), 
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

        return Tag.div(as, [
            Tag.hr(),
            Tag.div({style: {paddingRight: '20px'}}, xs),
            Tag.hr()
        ]); 
    }

    function userInfo() {
        return Me;
    }

    define({
        nav: nav,
        frame: frame,
        footer: footer,
        userInfo: userInfo
    });

}

var donorId = Auth.Left && 'anonymous' || Auth.Right;
var donorUrl = '/donor/' + donorId + '.json';
deps.push(donorUrl);
require(deps, onReady);
}

require(authDeps, onAuthReady);
 
