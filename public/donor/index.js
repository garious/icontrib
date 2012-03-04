var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js',
    '/ui/nav.js', 
    '/ui/chart.js', 
    '/ui/core.js'
];

var defaultUser = {
   description: 'Align with me to support underwater hockey in the United States!',
   alignedImageUrl: '/images/friends.png'
};

function onReady(E, L, NAV, CHART, CORE) {

    function alignButton(user) {
        return CORE.button({href: '/me/?donateTo=' + user.id, loud: true, text: 'Donate!'});
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
            var pct = CORE.h6(Math.round(1000 * x.shares / total) / 10 + '%');

            var cols = L.hug([
                E.div({style: {width: '70px', height: pct.height}}, [pct]),
                CORE.a({href: 'charity/?id=' + x.url}, x.name)
            ]);
            rows.push(cols);
        }
        return L.spoon(rows);
    }

    function distributionTable(user) {
        if (user.funds) {
            var rows = [];
            for (var i = 0; i < user.funds.length; i++) {
                var row = L.spoon([
                    L.hug([
                        CORE.h4(user.funds[i].name),
                        L.pillow(390, 0),
                        alignButton({id: user.funds[i].labels[0]})
                    ]),
                    L.hug([
                        CHART.pie1(user),
                        fundContents(user.distribution, user.funds[i].name)
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
                CORE.h3(user.firstName + ' ' + user.lastName),
                E.span({style: {color: 'red'}}, [
                    CORE.h5('Helps raise $' + Math.round(user.alignedDonated / 100) + ' per month')
                ])
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

