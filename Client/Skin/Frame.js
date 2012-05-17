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

function onReady(Iface, Tag, ToDom, Observable, Webpage, Layout, Core, Colors) { 

    function loginWidget(as) {
 
        // Control the visibility of the the menu
        var visibility = Observable.observe('hidden');

        function onMouseOver() {
            visibility.set('visible');
        }

        function onMouseOut() {
            visibility.set('hidden');
        }

        if (as.auth.Left) {

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
                'float': 'right',
                cssFloat: 'right' // Required by Firefox and Opera
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
                Tag.tag({
                    name: 'div',
                    style: {
                        'float': 'right',
                        cssFloat: 'right',  // Required by Firefox and Opera
                        padding: '15px 5px'
                    },
                    contents: [logoutButton]
                })
            ];

            var tab = Tag.tag({name: 'div', style: tabStyle, contents: tabContents});

            return Tag.tag({
                name: 'div',
                style: {position: 'relative'},
                contents: [tab, menu],
                handlers: {mouseover: onMouseOver, mouseout: onMouseOut}
            });
        }
    }

    function nav(as) {
        var logo = Tag.tag({
            name: 'a',
            attributes: {href: '/'},
            style: {position: 'absolute'},
            contents: [
                Core.image({url: Yoink.baseUrl + '/logo.png', text: 'IContrib Home'})
            ]
        });

        return Tag.tag({
            name: 'div',
            contents: [
                Layout.pillow(0, 20),
                logo,
                loginWidget(as),
                Core.hr({width: 960, height: 4, color: Colors.green})
            ]
        });
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

    function frame(as) {
        var thumbnail;

        if (as.auth.Right) {
            var user = as.auth.Right;
            var userName = user.firstName && user.lastName ? (user.firstName + ' ' + user.lastName) : user.email;
            var img  = user.imageUrl ? Core.image({width: 50, height: 50, url: user.imageUrl, text: userName}) : Layout.pillow(1, 50);
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

            thumbnail = Tag.tag({
                name: 'a',
                attributes: {href: '/Me'},
                style: {textDecoration: 'none'},
                contents: [thumbContents]
            });
        }

        var navbar = nav({thumbnail: thumbnail, auth: as.auth});
        var body = Tag.tag({name: 'div', contents: [as.contents]});

        var node = Tag.tag({
            name: 'div',
            style: {margin: 'auto', width: '960px'},
            contents: [
                Layout.spoon([
                    navbar, 
                    Layout.pillow(50), 
                    body
                ])
            ]
        });

        return webpage(node);
    }

    function footer(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }

        return Tag.tag({
            name: 'div',
            attributes: as,
            style: {
                bottom: '0px',
                width: '100%',
                textAlign: 'center'
            },
            contents: [
                Core.hr(),
                Tag.tag({name: 'div', style: {padding: '20px'}, contents: xs})
            ]
        }); 
    }

    Yoink.define({
        nav: nav,
        frame: frame,
        footer: footer,
        webpage: webpage,
        post: post
    });

}

Yoink.require(deps, onReady);

