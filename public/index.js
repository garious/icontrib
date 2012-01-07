var deps = [
    'tag/tag.js',
    'widgets/nav.js', 
];

function callback(E, NAV) {

    function main() {

        var loader = YOINK.resourceLoader();

        var nav = NAV.nav();
        var content = E.div();

        var mkHandler = function(url) {
            return function() {
                loader.getResource(url, function(MOD) {
                    content.innerHTML = '';
                    content.appendChild(MOD.body());
                });
            };
        };

        var mainHandler = mkHandler('widgets/indexbody.js');

        var dock = NAV.dock( /*{align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300},*/ [
            NAV.dockItem({onclick: mainHandler, src: 'images/home.png', title: "Home"}),
            NAV.dockItem({onclick: mkHandler('widgets/donorbody.js'), src: 'images/portfolio.png', title: "Your Portfolio"}),
            NAV.dockItem({onclick: mkHandler('widgets/charitybody.js'), src: 'images/link.png', title: "Charities"}),
            NAV.dockItem({onclick: mkHandler('widgets/contactbody.js'), src: 'images/rss.png', title: "Keep Informed"}),
        ]);

        mainHandler();

        return E.div([nav, content, dock]);
    };

    return {
        title: "IContrib - Improve the world today.",
        main: main,
    };
};

return {
    deps: deps,
    callback: callback,
};

