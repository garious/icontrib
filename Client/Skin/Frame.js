var authDeps = [
    '/auth/check.json'
];

var deps = [
    '/Tag/Tag.js', 
    '/Tag/ToDom.js', 
    '/Tag/Webpage.js', 
    '/Tag/Layout.js', 
    'Core.js',
    'Colors.js'
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
function onReady(Tag, ToDom, Webpage, Layout, Core, Colors, Me) { 

    function loginWidget(as) {

        if (Auth.Left) {
            var onLogin = function (evt) {
                evt.preventDefault();
                window.location = '/LogIn';
            };

            var onSignup = function (evt) {
                evt.preventDefault();
                window.location = '/SignUp';
            };

            return Layout.hug([
                Layout.pillow(210, 0),
                Layout.spoon({align: 'right'}, [
                    Core.button({text: 'Log in', onClick: onLogin}),
                    Layout.pillow(0, 5),
                    Core.button({text: 'Sign up', onClick: onSignup}),
                    Layout.pillow(0, 15)
                ])
            ]);

        } else {
            var onClick = function (evt) {
                evt.preventDefault();
                post('/auth/logout', {}, function(data) {
                    window.location = '/';
                });
            };

            //var logoutButton = Core.hyperlink({url: '#', text: 'Sign out'});
            var logoutButton = Core.image({url: baseUrl + '/arrowdown-darkgreen.png', text: 'settings', onClick: onClick});

            return Tag.div({
                style: {
                    width: '270px',
                    height: '77px',
                    backgroundColor: '#eee',
                    borderRadius: '5px 5px 0px 0px',
                    border: '1px solid',
                    borderBottomWidth: '0px',
                    borderColor: Colors.lightColor
                }
            }, [
                Layout.spoon([
                    Layout.pillow(0, 15),
                    Layout.hug([
                        Layout.pillow(20, 0),
                        as.thumbnail,
                        Layout.pillow(20, 0),
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
            Core.image({url: baseUrl + '/logo.png', text: 'IContrib Home'})
        ]);

        return Layout.spoon([
            Layout.pillow(0, 20),
            Layout.hug([ // TODO: Create a z-stack
                logo,
                Layout.pillow(559, 0),
                loginWidget(as)
            ]),
            Core.hr({width: 960, height: 4, color: Colors.green})
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

    webpage.interfaces = {};

    webpage.interfaces[ToDom.toDomId] = {
        toDom: function (me) {
            return me.domNode;
        }
    };

    webpage.interfaces[Webpage.webpageId] = {
        getTitle: function (me) {
            return 'IContrib.org';
        }
    };

    function frame(contents) {
        var as = {};

        if (Auth.Right) {
            var thumbContents = Layout.hug([
                Core.image({width: 50, height: 50, url: Me.imageUrl, text: Me.firstName + ' ' + Me.lastName}),
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

            var thumbnail = Tag.a({href: '/Me', style: {width: dim.width + 'px', height: dim.height + 'px', textDecoration: 'none'}}, [
                thumbContents
            ]);

            as.thumbnail = thumbnail;
        }

        var navbar = nav({thumbnail: thumbnail});
        var body = Tag.div([contents]);

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
            Core.hr(),
            Tag.div({style: {padding: '20px'}}, xs)
        ]); 
    }

    function userInfo() {
        return Me;
    }

    define({
        nav: nav,
        frame: frame,
        footer: footer,
        userInfo: userInfo,
        webpage: webpage,
        post: post
    });

}

var donorId = Auth.Left && 'anonymous' || Auth.Right;
var donorUrl = '/donor/' + donorId + '.json';
deps.push(donorUrl);
require(deps, onReady);
}

require(authDeps, onAuthReady);
 
