var deps = [ 
    '../tag/tag.js', 
    '../login/login.js'
];

function onReady(E, L) {
    var body = E.div([ 
        L.loginForm("/login_charity/","/check_charity/")
    ]);
    return {
        title: "boxes are foxes.",
        main:   body
    };
}

define(deps, onReady);

