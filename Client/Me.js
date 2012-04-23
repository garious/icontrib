var deps = [
    '/Tag/Interface.js', 
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Tag/Observable.js', 
    '/Skin/Frame.js',
    '/Skin/Core.js',
    '/Skin/Donor.js',
    '/Skin/Chart.js',
    '/Skin/Slider.js',
    '/Skin/Colors.js',
    '/charity/popular.json'
];

function onReady(Iface, Tag, Layout, Observable, Frame, Core, Donor, Chart, Slider, Colors, Popular) { 

    function fundContents(dist, inputs, colors) {
        var rows = [];
        function mkHandler(j) {
            return function(evt) {
                var n = parseFloat(evt.target.value);
                if (n !== NaN && n > 0 && n < 100) {
                    var old = dist[j].shares;
                    var diff = n - old;
                    dist[j].shares = n;

                    for (var i = 0; i < inputs.length; i += 1) {
                        if (i !== j) {
                            var d = dist[i];
                            var v = d.shares - diff * d.shares/(100 - old);
                            d.shares = v;
                            inputs[i].set(v);
                        } else {
                            inputs[i].set(evt.target.value);
                        }
                    }
                }
            };
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];
            var obs = inputs[j];
            var percentage = Observable.thunk([obs], function(n){return Math.round(n * 10) / 10 + '%';});
            var color = colors[j % colors.length];

            var cols = Layout.hug([
                Core.hyperlink({url: '/Charity?id=' + x.cid, text: x.name, marginTop: 6, marginRight: 10}),
                Slider.slider({value: obs, width: 200, height: 4, color: color, marginTop: 10, marginBottom: 10, marginLeft: 10, marginRight: 10, onChange: mkHandler(j)}),
                Layout.pillow(10, 0),
                Core.input({type: 'text', size: 5, disabled: true, value: percentage, onKeyUp: mkHandler(j)})
            ]);
            rows.push(cols);
            rows.push(Layout.pillow(0,15));
        }

        return Layout.spoon({align: 'right'}, rows);
    }

    function distributionTable(dist, inputs, colors) {
        return Layout.hug({width: 550}, [Layout.pillow(30), fundContents(dist, inputs, colors)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var rows = [];
        var inputs = [];
        var dist = user.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];
            var pct = x.shares * 100 / total;
            var obs = Observable.observe(pct);
            inputs.push(obs);
        }

        var colors = Colors.dashboardColors;
        var pie = Chart.pie({distribution: user.distribution, height: 220, padding: 15, colors: colors}, inputs);
        var pieTin = Tag.tag('div', {style: {margin: 'auto 0px', width: '100%', textAlign: 'center'}}, [pie]);

        if (user.distribution.length > 0) {
            rows.push( pieTin );
            rows.push( Layout.pillow(0, 20) );
        }

        var fundingRows = [
            distributionTable(user.distribution, inputs, colors),
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
 
