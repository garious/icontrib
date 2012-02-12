var deps = [
    '/tag/tag.js',
    '/ui/nav.js'
];

function onReady(E, NAV) {

    function body() {
        return E.div([
            E.div(['Sign up, improve the world today.']),
            E.div([
                E.form({method: "post", action: "/addUser"}, [
                   'Name:',     E.br(), E.input({type: 'text', length: 10, name: 'nameS'}), E.br(),
                   'Email:',    E.br(), E.input({type: 'text', length: 10, name: 'emailS'}), E.br(),
                   'Password:', E.br(), E.input({type: 'password', length: 10, name: 'passwordS'}), E.br(),
                   'Re-type Password:', E.br(), E.input({type: 'password', length: 10, name: 'password_reS'}), E.br(),
                   E.input({type: 'submit', value: 'Submit'})
                ])
            ]),
            E.p({align: 'center', sytle: 'font-size:24px; font-weight:bold;'}, [E.br(), E.br(), E.br(), E.br(), E.br(), 'or']),
            E.div([
                E.br(), E.br(), E.br(), E.br(),
                E.img({src: 'login-cloud.png', alt: 'Log in up above...'})
            ])
        ]);
    }

    define( NAV.frame([body()]) );
}

require(deps, onReady);

