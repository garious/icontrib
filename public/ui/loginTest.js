var deps = [ 
    'login.js'
];

function onReady(LOGIN) {
    define({
        title: "boxes are foxes.",
        main:   LOGIN.loginForm({root:"/auth"})
    });
}

require(deps, onReady);

