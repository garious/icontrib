var dom = require('poochie/dom');
var layout = require('poochie/layout');
var chart = require('./chart');
var colors = require('./colors');
var core = require('./core');

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
    var rows = [layout.gap(15)];

    for (var j = 0; j < xs.length; j++) {
        var x = xs[j];
        var pct = core.h6(Math.round(1000 * x.shares / total) / 10 + '%');


        var cols = layout.hcat([
            dom.element({
                name: 'div',
                style: {width: '18px', height: '18px', backgroundColor: colors[j % colors.length]}
            }),
            layout.gap(15),
            dom.element({
                name: 'div',
                style: {width: '55px', height: pct.height},
                contents: [pct]
            }),
            core.hyperlink({url: 'charity?id=' + x.cid, text: x.name})
        ]);
        rows.push(cols);
    }
    return layout.vcat(rows);
}

function distributionTable(dist) {
    // inplace sort.  TODO: Probably should clone the array
    dist.sort(function(a,b){return b.shares - a.shares;});

    var total = 0;
    for (var j = 0; j < dist.length; j++) {
        var d = dist[j];
        total += d.shares;
    }

    var row = dom.element({
        name: 'div',
        style: {width: '100%'},
        contents: [
            layout.hcat([
                fundContents(dist, total, colors.dashboardColors),
                layout.gap(20),
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
    var userInfo = layout.hcat([
        layout.gap(25),
        core.h3(name)
    ]);

    return layout.vcat([
        layout.hcat([
            dom.element({
                name: 'img',
                style: {width: '90px', height: '90px'},
                attributes: {src: user.imageUrl, alt: name}
            }),
            userInfo
        ]),
        layout.gap(10),
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
        listItems.push( layout.gap(10) );
        listItems.push( core.hr({width: width - 4, marginLeft: -15}) );
        listItems.push( layout.gap(10) );

        var e = layout.hcat([
            dom.element({
                name: 'img',
                attributes: {src: x.imageUrl},
                style: {width: '50px', height: '50px'}
            }),
            layout.gap(20),
            core.hyperlink({url: '/charity?id=' + x.cid, text: x.name})
        ]);

        listItems.push(e);
    }

    return core.box({
        width: width,
        contents: layout.vcat(listItems)
    });
}

module.exports = {
    profile: profile,
    recommendedFunds: recommendedFunds,
    alignButton: alignButton
};
