var deps = [
    '/tag/tag.js', 
    '/ui/nav.js', 
    'body.html'
];

function onReady(E, NAV, html) {
    
    function body() {
        var div = E.div();
        div.innerHTML = html;
        return div;
    }

    define({
        title: "IContrib - Improve the world today.",
        body: body,
        main: NAV.frame([
            body()
        ])
    });
}

require(deps, onReady);

