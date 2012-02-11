var deps = [ 
    'login.js'
];

function onReady(LOGIN) {
    define(LOGIN.loginForm({root:"/auth"}));
}

require(deps, onReady);

