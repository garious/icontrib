var deps = [
    '/yoink/tag.js',
    '/yoink/layout.js',
    '/skin/chart.js',
    '/skin/colors.js',
    '/skin/core.js'
];

function onReady(tag, layout, chart, colors, core) {

    function alignButton(user) {
        return core.button({href: '/me?donateTo=' + user.id, loud: true, text: 'Donate!'});
    }

    function isMember(xs, x) {
        for (var i = 0; i < xs.length; i++) {
            if ( xs[i] === x ) {
                return true;
            }
        }
        return false;
    }

    function fundContents(xs, total, colors) {
        var rows = [layout.pillow(0, 15)];

        for (var j = 0; j < xs.length; j++) {
            var x = xs[j];
            var pct = core.h6(Math.round(1000 * x.shares / total) / 10 + '%');


            var cols = layout.hug([
                tag.tag({
                    name: 'div',
                    style: {width: '18px', height: '18px', backgroundColor: colors[j % colors.length]}
                }),
                layout.pillow(15, 0),
                tag.tag({
                    name: 'div',
                    style: {width: '55px', height: pct.height},
                    contents: [pct]
                }),
                core.hyperlink({url: 'charity?id=' + x.cid, text: x.name})
            ]);
            rows.push(cols);
        }
        return layout.spoon(rows);
    }

    function distributionTable(dist) {
        // inplace sort.  TODO: Probably should clone the array
        dist.sort(function(a,b){return b.shares - a.shares;});

        var total = 0;
        for (var j = 0; j < dist.length; j++) {
            var d = dist[j];
            total += d.shares;
        }

        var row = tag.tag({
            name: 'div',
            style: {width: '100%'},
            contents: [
                layout.hug([
                    fundContents(dist, total, colors.dashboardColors),
                    layout.pillow(20, 0),
                    chart.pieSnapshot({distribution: dist, colors: colors.dashboardColors, height: 200})
                ])
            ]
        });

        return row;
    }

    function profile(as) {
        as = as || {};
        var user = as.user || {};
        var name = user.firstName + ' ' + user.lastName;
        var userInfo = layout.hug([
            layout.pillow(25, 0), 
            core.h3(name)
        ]);

        return layout.spoon([
            layout.hug([
                tag.tag({
                    name: 'img',
                    style: {width: '90px', height: '90px'},
                    attributes: {src: user.imageUrl, alt: name}
                }),
	        userInfo
            ]),
            layout.pillow(0, 10),
            distributionTable(user.distribution)
        ]);
    }

    function recommendedFunds(as) {

        var width = as.width || 340;

        var listItems = [
            core.h5({
                color: colors.greenText,
                text: 'Recommended Funds'
            })
        ];

        for (var i = 0; i < as.funds.length; i += 1) {
            var x = as.funds[i];
            listItems.push( layout.pillow(0, 10) );
            listItems.push( core.hr({width: width - 4, marginLeft: -15}) );
            listItems.push( layout.pillow(0, 10) );

            var e = layout.hug([
                tag.tag({
                    name: 'img',
                    attributes: {src: x.imageUrl},
                    style: {width: '50px', height: '50px'}
                }),
                layout.pillow(20, 0),
                core.hyperlink({url: '/charity?id=' + x.cid, text: x.name})
            ]);

            listItems.push(e);
        }

        return core.box({
            width: width,
            contents: layout.spoon(listItems)
        });
    }

    yoink.define({
        profile: profile,
        recommendedFunds: recommendedFunds,
        alignButton: alignButton
    });
}

yoink.require(deps, onReady);

