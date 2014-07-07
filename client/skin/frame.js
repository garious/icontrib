var deps = [
    '/stdlib/tag.js',
    '/stdlib/observable.js',
    '/stdlib/layout.js',
    'core.js',
    'colors.js'
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

function onReady(tag, observable, layout, core, colors) {

    function loginWidget(as) {

        // Control the visibility of the the menu
        var visibility = observable.observe('hidden');

        function onMouseOver() {
            visibility.set('visible');
        }

        function onMouseOut() {
            visibility.set('hidden');
        }

        if (as.auth.Left) {
            var loginData = as.auth.Left;

            var onLogin = function (evt) {
                evt.preventDefault();
                window.location = loginData.loginUrl;
            };

            return layout.vcat({align: 'right'}, [
                core.button({text: 'Log in', onClick: onLogin, quiet: true}),
                layout.gap(0, 20)
            ]);

        } else {
            var user = as.auth.Right;
            var logoutButton = core.image({url: yoink.baseUrl + '/arrowdown.png', text: 'settings'});

            var tabStyle = {
                width: '270px',
                backgroundColor: '#eee',
                borderRadius: '5px 5px 0px 0px',
                border: '1px solid',
                borderBottomWidth: '0px',
                borderColor: colors.lightColor,
                padding: '15px 5px',
                cssFloat: 'right'
            };

            var logoff = function (evt) {
                evt.preventDefault();
                window.location = user.logoutUrl;
            };

            var menu = core.menu({
                width: 280,
                top: 80,
                visibility: visibility,
                menuItems: [
                    core.menuItem({contents: core.h6('Manage my distribution'), onSelect: '/me'}),
                    core.menuItem({contents: core.h6('Log off'),  onSelect: logoff})
                ]
            });

            var tabContents = [
                as.thumbnail,
                tag.tag({
                    name: 'div',
                    style: {
                        cssFloat: 'right',
                        padding: '15px 5px'
                    },
                    contents: [logoutButton]
                })
            ];

            var tab = tag.tag({name: 'div', style: tabStyle, contents: tabContents});

            return tag.tag({
                name: 'div',
                style: {position: 'relative'},
                contents: [tab, menu],
                handlers: {mouseover: onMouseOver, mouseout: onMouseOut}
            });
        }
    }

    function nav(as) {
        var logo = tag.tag({
            name: 'a',
            attributes: {href: '/'},
            style: {position: 'absolute'},
            contents: [
                core.image({url: yoink.baseUrl + '/logo.png', text: 'IContrib Home'})
            ]
        });

        return tag.tag({
            name: 'div',
            contents: [
                layout.gap(0, 20),
                logo,
                loginWidget(as),
                core.hr({width: 960, height: 4, color: colors.green})
            ]
        });
    }

    //
    // Web Page object
    //
    function webpage(domable) {
        return {
            constructor: webpage,
            toDom: function (me) {
                return domable.toDom ? domable.toDom() : domable;
            },
            getTitle: function (me) {
                return 'IContrib.org';
            }
        };
    }

    function frame(as) {
        var thumbnail;

        if (as.auth.Right) {
            var user = as.auth.Right;
            var userName = user.firstName && user.lastName ? (user.firstName + ' ' + user.lastName) : user.email;
            var img  = user.imageUrl ? core.image({width: 50, height: 50, url: user.imageUrl, text: userName}) : layout.gap(1, 50);
            var thumbContents = layout.hcat([
                img,
                layout.gap(20, 0),
                layout.vcat([
                    layout.gap(0, 10),
                    core.h3({
                        color: colors.greenText,
                        text: userName
                    })
                ])
            ]);

            thumbnail = tag.tag({
                name: 'a',
                attributes: {href: '/me'},
                style: {textDecoration: 'none'},
                contents: [thumbContents]
            });
        }

        var navbar = nav({thumbnail: thumbnail, auth: as.auth});
        var body = tag.tag({name: 'div', contents: [as.contents]});

        var node = tag.tag({
            name: 'div',
            style: {margin: 'auto', width: '960px'},
            contents: [
                layout.vcat([
                    navbar,
                    layout.gap(50),
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

        return tag.tag({
            name: 'div',
            attributes: as,
            style: {
                bottom: '0px',
                width: '100%',
                textAlign: 'center'
            },
            contents: [
                core.hr(),
                tag.tag({name: 'div', style: {padding: '20px'}, contents: xs})
            ]
        });
    }

    yoink.define({
        nav: nav,
        frame: frame,
        footer: footer,
        webpage: webpage,
        post: post
    });

}

yoink.require(deps, onReady);

