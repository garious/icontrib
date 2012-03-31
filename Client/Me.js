var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js',
    '/Skin/Core.js',
    '/Skin/Donor.js',
    '/Skin/Chart.js',
    '/charity/popular.json'
];

function onReady(Tag, Layout, Frame, Core, Donor, Chart, Popular) { 

    function fundContents(pie) {
        var dist = pie.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        var rows = [];
        var inputs = [];

        function mkHandler(j) {
            return function(evt) {
                var n = parseFloat(evt.target.value);
                if (n !== NaN && n > 0 && n < 100) {
                    var old = pie.distribution[j].shares;
                    var diff = n - old;
                    pie.distribution[j].shares = n;

                    for (var i = 0; i < inputs.length; i += 1) {
                        if (i !== j) {
                            var d = dist[i];
                            var v = d.shares - diff * d.shares/(100 - old);
                            d.shares = v;
                            var pct = Math.round(v * 100) / 100;
                            inputs[i].value = pct; 
                        }
                    }
                    pie.draw();
                } //else {
                    // TODO: disable 'Save Changes'
                //}
            };
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];

            var pct = Math.round(x.shares / total * 1000) / 10;
            var e = Core.input({type: 'text', size: 4, value: pct, onKeyUp: mkHandler(j)});

            inputs.push(e);
            var cols = Layout.hug([
                e,
                Layout.pillow(10, 0),
                Core.hyperlink({url: x.url, text: x.name})
            ]);
            rows.push(cols);
            rows.push(Layout.pillow(0,15));
        }
        return Layout.spoon(rows);
    }

    function distributionTable(pie) {
        return Layout.hug({width: 550}, [Layout.pillow(30), fundContents(pie)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var alignedUsers = user.alignedUsers || [];
        var raised = Math.round(user.alignedDonated / 100);
        var impactMsg = alignedUsers.length + ' donors are aligned with your distribution.  Together you help raise $' + raised + ' per month!';

        var rows = [];

        var impactHeader = Core.h6({text: impactMsg, maxWidth: 550});

        if (alignedUsers.length > 0) {
            rows.push( Core.h3('My impact') );
            rows.push( Layout.hug([Layout.pillow(30), impactHeader]) );
        }

        var pie = Chart.pie(user);

        if (user.distribution.length > 0) {
            rows.push( Core.h3('My charitable distribution') );
            rows.push( Layout.hug([Layout.pillow(100), pie.element]) );
        }

        var fundingRows = [
            distributionTable(pie),
            Core.h3('My funding'),
            Layout.pillow(0, 20),
            Layout.hug([Layout.pillow(30, 0), Core.input({type: 'text', size: 10, value: user.centsDonated / 100.0}), Layout.pillow(10,0), Core.h6("dollars per month")]),
            Layout.pillow(0, 20),
            Layout.hug([Layout.pillow(20,0), Core.button({href: '#', text: 'Save Changes', loud: true})]),
            Layout.pillow(0, 20)
        ];

        return Layout.spoon(rows.concat(fundingRows));
    }

    var main = Frame.frame(
        Layout.spoon([
            Layout.hug([
                Core.box({
                    width: 600,
                    contents: dashboard({user: Frame.userInfo()})
                }),
                Layout.pillow(20),
                Donor.recommendedFunds({funds: Popular})
            ]),
            Layout.pillow(20) 
        ]));

    define(main);
}

require(deps, onReady);
 
