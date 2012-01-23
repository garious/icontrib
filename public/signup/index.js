var deps = [
    '/tag/tag.js',
    '/ui/nav.js'
];

function onReady(E, NAV) {
    function body() {
        return E.div({id: 'content', 'class': 'container_12'}, [
            E.div({id: 'call-to-action'}, ['Sign up, improve the world today.']),
            E.div({'class': 'grid_12'}),
            E.div({'class': 'grid_5 grey-box'}, [
                E.form({method: "post", action: "/addUser"}, [
                   'Name:',     E.br(), E.input({type: 'text', length: 10, name: 'nameS'}), E.br(),
                   'Email:',    E.br(), E.input({type: 'text', length: 10, name: 'emailS'}), E.br(),
                   'Password:', E.br(), E.input({type: 'password', length: 10, name: 'passwordS'}), E.br(),
                   'Re-type Password:', E.br(), E.input({type: 'password', length: 10, name: 'password_reS'}), E.br(),
                   E.input({type: 'submit', value: 'Submit'})
                ])
            ]),
            E.div({'class': 'grid_1'}, [
                E.p({align: 'center', sytle: 'font-size:24px; font-weight:bold;'}, [E.br(), E.br(), E.br(), E.br(), E.br(), 'or'])
            ]),
            E.div({'class': 'grid_5'}, [
                E.br(), E.br(), E.br(), E.br(),
                E.img({src: 'login-cloud.png', alt: 'Log in up above...'})
            ])
        ]);
    }

    function main() {
        return NAV.frame([
            body()
        ]);
    }

    define({
        title: 'IContrib Signup',
        body: body,
        main: main
    });
}

require(deps, onReady);

