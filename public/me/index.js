var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js',
    '/ui/core.js',
    '/ui/chart.js',
    '/charity/popular.json'
];

function onReady(E, L, NAV, CORE, CHART, POP) { 

    function fundContents(pie) {
        var dist = pie.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        var rows = [];
        var inputs = [];

        function mkHandler(e, j) {
            return function(evt) {
                var n = parseFloat(e.value);
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
            var e = CORE.input({type: 'text', size: 4, value: pct});
            e.addEventListener('keyup', mkHandler(e, j));

            inputs.push(e);
            var cols = L.hug([
                e,
                L.pillow(10, 0),
                CORE.a({href: x.url}, x.name)
            ]);
            rows.push(cols);
            rows.push(L.pillow(0,15));
        }
        return L.spoon(rows);
    }

    function distributionTable(pie) {
        return L.hug({style: {width: 550}}, [L.pillow(30), fundContents(pie)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var alignedUsers = user.alignedUsers || [];
        var raised = Math.round(user.alignedDonated / 100);
        var impactMsg = alignedUsers.length + ' donors are aligned with your distribution.  Together you help raise $' + raised + ' per month!';

        var rows = [];

        // TODO: clean up calculation of size of string that wraps over serveral lines
        var impactHeader = CORE.h6(impactMsg);
        impactHeader.style.width = '550px';
        impactHeader.style.height = '50px';

        if (alignedUsers.length > 0) {
            rows.push( CORE.h3('My impact') );
            //rows.push( L.hug([L.pillow(30), CORE.h6(impactMsg)]) );
            rows.push( L.hug([L.pillow(30), impactHeader]) );
        }

        var pie = CHART.pie(user);

        if (user.distribution.length > 0) {
            rows.push( CORE.h3('My charitable distribution') );
            rows.push( L.hug([L.pillow(100), pie.element]) );
        }

        var fundingRows = [
            distributionTable(pie),
            CORE.h3('My funding'),
            L.pillow(0, 20),
            L.hug([L.pillow(30, 0), CORE.input({type: 'text', size: 10, value: user.centsDonated / 100.0}), L.pillow(10,0), CORE.h6("dollars per month")]),
            L.pillow(0, 20),
            L.hug([L.pillow(20,0), CORE.button({href: '#', text: 'Save Changes', loud: true})]),
            L.pillow(0, 20)
        ];

        return L.spoon(rows.concat(fundingRows));
    }

    var listItems = [
        CORE.h2('Recommended Funds'),
        L.pillow(0, 10)
    ];

    for (var i = 0; i < POP.length; i += 1) {
        var x = POP[i];
        listItems.push( CORE.a({href: '/charity/?id=' + x.id}, x.name) );
    }

    var main = NAV.frame([
        L.spoon([
            L.hug([
                L.pillow(200),
                CORE.box({width: '600px'}, [
                    dashboard({user: NAV.userInfo()})
                ]),
                L.pillow(20),
                CORE.box(L.spoon(listItems))
            ]),
            L.pillow(20) 
        ])
    ]);

    define(main);
}

require(deps, onReady);
 
