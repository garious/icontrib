var deps = [ 
    'login.js'
];

function onReady(LOGIN) {
    return {
        title: "boxes are foxes.",
        main:   LOGIN.loginForm("/login_charity/")
    };
}

define(deps, onReady);

