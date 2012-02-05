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
        var alignLink = CORE.button({href: '/me/?donateTo=' + user.id}, ['Donate!']);
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
                E.td({style: {textAlign: 'right'}}, [CORE.h6(Math.round(1000 * x.shares / total) / 10 + '%')]),
                E.td([CORE.a({href: x.url}, x.name)])
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
                var row = E.div({style: {width: 550}}, [
                    L.spoon([
                        L.hug([
                            CORE.h4(user.funds[i].name),
                            L.pillow(300, 0),
                            alignButton({id: user.funds[i].labels[0]})
                        ]),
                        L.hug([
                            CHART.pie1(user),
                            L.hug([fundContents(user.distribution, user.funds[i].name)])
                        ])
                    ])
                ]);
                rows.push(row);
            }
            return L.spoon(rows, 20);
        }
    }

    function profile(as) {
        as = as || {};
        var user = as.user || {};
        var userInfo = L.hug([
            L.pillow(25, 0), 
            L.spoon([
                CORE.h3([user.firstName + ' ' + user.lastName]),
                CORE.h5('Helps raise $' + Math.round(user.alignedDonated / 100) + ' per month')
            ])
        ]);

        return L.spoon([
            L.hug([
                E.img({style: {width: '70px', height: '90px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
	        userInfo
            ]),
            L.pillow(0, 20),
            distributionTable(user)
        ]);
    }

    define({
        title: 'IContrib.org',
        profile: profile,
        alignButton: alignButton
    });
}

require(deps, onReady);

