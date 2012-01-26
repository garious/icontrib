var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js',
    '/ui/core.js',
    '/ui/chart.js',
    '/jquery/jquery-ui-mod.js'
];

function onReady(E, L, NAV, CORE, CHART, $UI) { 

    function fundContents(dist) {
        var rows = [];

        for (var j = 0; j < dist.length; j++) {
            var x = dist[j];

            var sldr = E.div({style: {width: 200}});
            $UI(sldr).slider({value: x.shares});

            var cols = [
                E.td([CORE.a({href: x.url}, x.name)]),
                E.td([sldr])
            ];
            rows.push(E.tr(cols));
        }
        return E.table({cellSpacing: 10}, [
            E.link({type: "text/css", href: "/css/smoothness/jquery-ui-1.8.16.custom.css", rel: "stylesheet"}),
            E.tbody(rows)
        ]);
    }

    function distributionTable(user) {
        return L.hug({style: {width: 550}}, [L.pillow(30), fundContents(user.distribution)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var userChart = L.spoon([
            CORE.h3('My impact'),
            L.hug([L.pillow(30), CORE.h6((user.alignedUsers || []).length + ' donors are aligned with your distribution.  Together you help raise $' + user.alignedDonated + ' per month!')]),
            CORE.h3('My charitable distribution'),
            L.hug([L.pillow(100), CHART.pie(user)])
        ]);

        return L.spoon([
	    userChart,
            distributionTable(user),
            CORE.h3('My funding'),
            L.hug([L.pillow(30), E.input({type: 'text', value: user.dollarsDonated}), CORE.h6("dollars per month")], 10),
            CORE.button({href: '#'}, ['Save Changes']),
            L.pillow(30)
        ], 10);
    }

    function main() {
        return NAV.frame([
            L.hug([
                L.pillow(250),
                CORE.box([
                    dashboard({user: NAV.userInfo()})
                ])
            ])
        ]);
    }

    define({
        main: main
    });

}

require(deps, onReady);
 
