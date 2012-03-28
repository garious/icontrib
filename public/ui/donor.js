var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js',
    '/ui/chart.js', 
    '/ui/colors.js', 
    '/ui/core.js'
];

function onReady(Tag, Layout, Chart, Colors, Core) {

    function alignButton(user) {
        return Core.button({href: '/Me?donateTo=' + user.id, loud: true, text: 'Donate!'});
    }

    function isMember(xs, x) {
        for (var i = 0; i < xs.length; i++) {
            if ( xs[i] === x ) {
                return true;
            }
        }
        return false;
    }

    function fundContents(xs, total) {
        var rows = [Layout.pillow(0, 15)];

        var colors = [
            Colors.green,
            '#ddffaa'
        ];

        for (var j = 0; j < xs.length; j++) {
            var x = xs[j];
            var pct = Core.h6(Math.round(1000 * x.shares / total) / 10 + '%');


            var cols = Layout.hug([
                Tag.div({style: {width: '18px', height: '18px', backgroundColor: colors[j % colors.length]}}),
                Layout.pillow(15, 0),
                Tag.div({style: {width: '55px', height: pct.height}}, [pct]),
                Core.hyperlink({url: 'Charity?id=' + x.cid, text: x.name})
            ]);
            rows.push(cols);
        }
        return Layout.spoon(rows);
    }

    function distributionTable(user) {
        if (user.funds) {
            var rows = [];
            var dist = user.distribution;
            for (var i = 0; i < user.funds.length; i++) {

                var fundId = user.funds[i].labels[0];  // TODO: Anatoly, why is this a list?
                var xs = [];
                var total = 0;

                // filter (nm `elem` dist.labels)
                for (var j = 0; j < dist.length; j++) {
                    if (isMember(dist[j].labels, fundId)) {
                        var d = dist[j];
                        total = total + d.shares;
                        xs.push(d);
                    }
                }

                var row = Layout.spoon([
                    Core.hr({width: 570}),
                    Layout.pillow(0, 20),
                    Tag.div({style: {height: '30px'}}, [
                        Core.h4(user.funds[i].name),
                        Tag.div({style: {position: 'absolute', top: '10px', left: '505px'}}, [  // TODO: remove top 10px, which is due to the button falling outside its bounds
                            alignButton({id: fundId})
                        ])
                    ]),
                    Layout.hug([
                        Chart.pie1({distribution: xs}),
                        Layout.pillow(20, 0),
                        fundContents(xs, total)
                    ])
                ]);

                rows.push(row);
            }
            return Layout.spoon(rows);
        }
    }

    function profile(as) {
        as = as || {};
        var user = as.user || {};
        var userInfo = Layout.hug([
            Layout.pillow(25, 0), 
            Layout.spoon([
                Core.h3(user.firstName + ' ' + user.lastName),
                Core.h5({
                    color: 'red',
                    text: 'Helps raise $' + Math.round(user.alignedDonated / 100) + ' per month'
                })
            ])
        ]);

        return Layout.spoon([
            Layout.hug([
                Tag.img({style: {width: '90px', height: '90px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
	        userInfo
            ]),
            Layout.pillow(0, 10),
            distributionTable(user)
        ]);
    }

    function recommendedFunds(as) {

        var width = as.width || 340;

        var listItems = [
            Core.h5({
                color: Colors.greenText,
                text: 'Recommended Funds'
            })
        ];

        var pad = Layout.pillow(0, 10);

        for (var i = 0; i < as.funds.length; i += 1) {
            var x = as.funds[i];
            listItems.push( pad );
            listItems.push( Core.hr({width: width - 4, marginLeft: -15}) );
            listItems.push( pad );

            var e = Layout.hug([
                Tag.img({src: x.imageUrl, style: {width: '50px', height: '50px'}}),
                Layout.pillow(20, 0),
                Core.hyperlink({url: '/Charity?id=' + x.cid, text: x.name})
            ]);

            listItems.push(e);
        }

        return Core.box({
            width: width,
            contents: Layout.spoon(listItems)
        });
    }

    define({
        profile: profile,
        recommendedFunds: recommendedFunds,
        alignButton: alignButton
    });
}

require(deps, onReady);

