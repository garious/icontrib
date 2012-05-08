var authDeps = [
    '/auth/check.json'
];

var deps = [
    '/Tag/Interface.js', 
    '/Tag/Tag.js', 
    '/Tag/ToDom.js', 
    '/Tag/Observable.js', 
    '/Tag/Webpage.js', 
    '/Tag/Layout.js', 
    'Core.js',
    'Colors.js'
];


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
function onReady(Iface, Tag, ToDom, Observable, Webpage, Layout, Core, Colors, Me) { 

    function loginWidget(as) {
 
        // Control the visibility of the the menu
        var visibility = Observable.observe('hidden');

        function onMouseOver() {
            visibility.set('visible');
        }

        function onMouseOut() {
            visibility.set('hidden');
        }

        function invalidateBackCache() {
            // necessary for Safari: mobile & desktop
        }

        if (Auth.Left) {
            // necessary for Safari: mobile & desktop
            window.addEventListener('unload', invalidateBackCache, false);

            var onLogin = function (evt) {
                evt.preventDefault();
                window.location = '/LogIn';
            };

            var onSignup = function (evt) {
                evt.preventDefault();
                window.location = '/SignUp';
            };

            return Layout.spoon({align: 'right'}, [
                Core.button({text: 'Log in', onClick: onLogin, quiet: true}),
                Layout.pillow(0, 5),
                Core.button({text: 'Sign up', onClick: onSignup, quiet: true}),
                Layout.pillow(0, 15)
            ]);

        } else {
            var logoutButton = Core.image({url: Yoink.baseUrl + '/arrowdown-darkgreen.png', text: 'settings'});

            var tabStyle = {
                width: '270px',
                backgroundColor: '#eee',
                borderRadius: '5px 5px 0px 0px',
                border: '1px solid',
                borderBottomWidth: '0px',
                borderColor: Colors.lightColor,
                padding: '15px 5px',
                'float': 'right'
            };

            var logoff = function (evt) {
                evt.preventDefault();
                post('/auth/logout', {}, function(data) {
                    window.location = '/';
                });
            };

            var menu = Core.menu({
                width: 280,
                top: 80,
                visibility: visibility,
                menuItems: [  
                    Core.menuItem({contents: Core.h6('Manage my distribution'), onSelect: '/Me'}),
                    Core.menuItem({contents: Core.h6('Log off'),  onSelect: logoff})
                ]
            });

            var tabContents = [
                as.thumbnail,
                Tag.tag('div', {
                    style: {
                        'float': 'right',
                        padding: '15px 5px'
                    }
                }, [
                    logoutButton
                ])
            ];

            var tab = Tag.tag('div', {style: tabStyle}, tabContents);

            return Tag.tag('div', {style: {position: 'relative'}}, [tab, menu], {mouseover: onMouseOver, mouseout: onMouseOut});
        }
    }

    function nav(as) {
        as = as || {};

        var logo = Tag.tag('a', {href: '/', style: {position: 'absolute'}}, [
            Core.image({url: Yoink.baseUrl + '/logo.png', text: 'IContrib Home'})
        ]);

        return Tag.tag('div', [
            Layout.pillow(0, 20),
            logo,
            loginWidget(as),
            Core.hr({width: 960, height: 4, color: Colors.green})
        ]);
    }

    //
    // Web Page object
    //
    function webpage(domable) {
        return {
            constructor: webpage,
            domable: domable
        };
    }

    webpage.interfaces = {};

    webpage.interfaces[ToDom.toDomId] = {
        toDom: function (me) {
            var iface = Iface.getInterface(me.domable, ToDom.toDomId);
            return iface !== undefined ? iface.toDom(me.domable) : me.domable;
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
            var userName = Me.firstName && Me.lastName ? (Me.firstName + ' ' + Me.lastName) : Me.email;
            var img  = Me.imageUrl ? Core.image({width: 50, height: 50, url: Me.imageUrl, text: userName}) : Layout.pillow(1, 50);
            var thumbContents = Layout.hug([
                img,
                Layout.pillow(20, 0),
                Layout.spoon([
                    Layout.pillow(0, 10),
                    Core.h3({
                        color: Colors.greenText,
                        text: userName
                    })
                ])
            ]);

            var thumbnail = Tag.tag('a', {href: '/Me', style: {textDecoration: 'none'}}, [
                thumbContents
            ]);

            as.thumbnail = thumbnail;
        }

        var navbar = nav({thumbnail: thumbnail});
        var body = Tag.tag('div', [contents]);

        var node = Tag.tag('div', {style: {margin: 'auto', width: '960px'}}, [
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

        return Tag.tag('div', as, [
            Core.hr(),
            Tag.tag('div', {style: {padding: '20px'}}, xs)
        ]); 
    }

    function userInfo() {
        return Me;
    }

    Yoink.define({
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
Yoink.require(deps, onReady);
}

Yoink.require(authDeps, onAuthReady);
 
