var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js',
    '/ui/core.js',
    '/ui/chart.js'
];

function onReady(E, L, NAV, CORE, CHART) { 

    function fundContents(dist) {

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        var rows = [];

        for (var j = 0; j < dist.length; j++) {
            var x = dist[j];

            var pct = Math.round(x.shares / total * 1000) / 10;
            var cols = L.hug([
                CORE.input({type: 'text', size: 4, value: pct}),
                L.pillow(10, 0),
                CORE.a({href: x.url}, x.name)
            ]);
            rows.push(cols);
            rows.push(L.pillow(0,15));
        }
        return L.spoon(rows);
    }

    function distributionTable(user) {
        return L.hug({style: {width: 550}}, [L.pillow(30), fundContents(user.distribution)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var alignedUsers = user.alignedUsers || [];
        var raised = Math.round(user.alignedDonated / 100);
        var impactMsg = alignedUsers.length + ' donors are aligned with your distribution.  Together you help raise $' + raised + ' per month!';

        var userChart = L.spoon([
            CORE.h3('My impact'),
            L.hug([L.pillow(30), CORE.h6(impactMsg)]),
            CORE.h3('My charitable distribution'),
            L.hug([L.pillow(100), CHART.pie(user)])
        ]);

        return L.spoon([
	    userChart,
            distributionTable(user),
            CORE.h3('My funding'),
            L.pillow(0, 20),
            L.hug([L.pillow(30, 0), CORE.input({type: 'text', size: 10, value: user.centsDonated / 100.0}), L.pillow(10,0), CORE.h6("dollars per month")]),
            L.pillow(0, 20),
            L.hug([L.pillow(20,0), CORE.button({href: '#'}, ['Save Changes'])]),
            L.pillow(0, 20)
        ], 10);
    }

    function main() {
        return NAV.frame([
            L.spoon([
                L.hug([
                    L.pillow(250),
                    CORE.box([
                        dashboard({user: NAV.userInfo()})
                    ])
                ]),
                L.pillow(20) 
            ])
        ]);
    }

    define({
        main: main
    });

}

require(deps, onReady);
 
