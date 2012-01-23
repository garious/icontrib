var deps = [ 
    'login.js'
];

function onReady(LOGIN) {
    define({
        title: "boxes are foxes.",
        main:   LOGIN.loginForm("/login_charity/")
    });
}

require(deps, onReady);

