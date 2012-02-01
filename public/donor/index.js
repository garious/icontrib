var deps = [
    '/tag/tag.js', 
    '/tag/layout.js',
    '/ui/nav.js', 
    '/ui/chart.js', 
    '/ui/core.js',
    '/ui/colors.json',
    '/jquery/jquery-ui-mod.js'
];

var defaultUser = {
   description: 'Align with me to support underwater hockey in the United States!',
   alignedImageUrl: '/images/friends.png'
};

function onReady(E, L, NAV, CHART, CORE, C, $) { 

    function alignButton(user) {
        var alignLink = CORE.button({href: '/me/?donateTo=' + user.uid}, ['Donate!']);
        return alignLink;
        //alignLink.onclick = function(e) { 
        //    //TODO: On click, navigate to appropriate pages
        //    WAIT.load({
        //        buttons: {
        //            "Sign In" : function(e) { window.location ="/signup/"; },
        //            "Keep Adding Stuff" : function(e) { WAIT.close(); } 
        //        }, 
        //        title: "What do you want to do?",
        //        content: "From here, you can either sign up to fund your distribution, or continue selecting organizations you would like to support."
        //    });
        //};
        //var alignDiv = E.div([
        //   E.link({type: "text/css", href: "/css/smoothness/jquery-ui-1.8.16.custom.css", rel: "stylesheet"}),
        //   alignLink
        //]);
        //return alignDiv;
    }

    function isMember(xs, x) {
        for (var i = 0; i < xs.length; i++) {
            if ( xs[i] === x ) {
                return true;
            }
        }
        return false;
    }

    function fundContents(dist, nm) {
        var rows = [];
        var xs = [];
        var total = 0;

        // filter (nm `elem` dist.labels)
        for (var i = 0; i < dist.length; i++) {
            if (isMember(dist[i].labels, nm)) {
                var d = dist[i];
                total = total + d.shares;
                xs.push(d);
            }
        }

        for (var j = 0; j < xs.length; j++) {
            var x = xs[j];

            var cols = [
                E.td([CORE.a({href: x.url}, x.name)]),
                E.td({style: {textAlign: 'right'}}, [E.text(Math.round(1000 * x.shares / total) / 10 + '%')])
            ];
            rows.push(E.tr(cols));
        }
        return E.table({cellSpacing: 10}, [
            E.tbody(rows)
        ]);
    }

    function distributionTable(user) {
        if (user.funds) {
            var rows = [];
            for (var i = 0; i < user.funds.length; i++) {
                var row = CORE.box([E.div({style: {width: 550}}, [
                    L.hug([
                        L.spoon([
                            CORE.h4(user.funds[i].name),
                            L.hug([L.pillow(30), fundContents(user.distribution, user.funds[i].name)])
                        ], 25),
                        L.spoon([
                            L.pillow(50),
                            alignButton({id: user.funds[i].labels[0]})
                        ])
                    ], 100)
                ])]);
                rows.push(row);
            }
            return L.spoon(rows, 20);
        } else {
            return alignButton(user);
        }
    }

    function profile(as) {
        as = as || {};
        var user = as.user || {};
        var userChart = L.spoon([
            L.hug([L.pillow(50, 0), CORE.h3('Helps raise $' + user.alignedDonated + ' per month')]),
            CHART.pie(user)
        ]);

        return L.spoon([
            CORE.h2(as.title), 
            L.pillow(30),
            L.hug([
                L.spoon([
                    E.img({style: {width: '175px', height: '225px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
                    CORE.h3([user.firstName + ' ' + user.lastName])
                ], 20),
	        userChart
            ]),
            distributionTable(user)
        ]);
    }

    function TomBrown(params, nodeReady) {
        require(['tom.json'], function(user) {
            nodeReady( NAV.frame([profile({user: user})]) );
        });
    }

    define({
        title: 'IContrib.org',
        main: TomBrown,
        summary: profile,
        alignButton: alignButton,
        TomBrown: TomBrown
    });
}

require(deps, onReady);

