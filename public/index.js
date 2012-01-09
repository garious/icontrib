var deps = [
    'tag/tag.js', 
    'nav/index.js', 
    'indexBody.html' 
];

function onReady(E, NAV, html) {
    
    function body() {
        var div = E.div();
        div.innerHTML = html; 
        return div;
    };

    return {
        title: "IContrib - Improve the world today.",
        body: body,
        main: NAV.frame([
            body(),
        ])
    };
};

return {
    deps: deps,
    callback: onReady
};

