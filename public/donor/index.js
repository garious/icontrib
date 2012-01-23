// TODO: Generalize this function and move google's jsapi into a module
function exportGoogle(text, yoink, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', yoink, callback);
}

var deps = [
    '/tag/tag.js', 
    '/tag/layout.js',
    '/ui/nav.js', 
    {path: '/mirror/google/jsapi', interpreter: exportGoogle},
    '/widgets/waitScreen.js',
    '/ui/core.js',
    '/ui/colors.json'
];

var defaultUser = {
   firstName: 'Greg',
   lastName: 'Fitzgerald',
   description: 'Align with me to support underwater hockey in the United States!',
   imageUrl: '/images/gregf.jpg',
   dollarsDonated: 2456,
   alignedDonated: 12456,
   alignedImageUrl: '/images/friends.png',
   distribution: [
       ['UNICEF', 35],
       ['American Red Cross', 10],
       ['La Jolla Playhouse', 10],
       ['San Diego Foundation', 10], 
       ['USA UWH', 80],
       ['LACC', 20]
   ]
};

function onReady(E, L, NAV, google, wait, CORE, C) { 

    function chart(user) {
        user = user || defaultUser;
        var dist = [];
        for (var i = 0; i < user.distribution.length; i++) {
            var ud = user.distribution[i]; 
            dist.push([ud.name, ud.shares]); 
        }
        var userChart = E.div([
            E.img({src: '/images/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto', width: '400px', height: '300px'}})
        ]);
   
        var cookPie = function() {
            var options = {width: 400, height: 300, backgroundColor: { fill:'transparent' }};
            options.colors = [C.darkColor, C.middleColor, C.lightColor];
            var chart = new google.visualization.PieChart(userChart);
            var data = new google.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            data.addRows(dist);
            chart.draw(data, options);
        };
        google.load('visualization', '1.0', {packages:['corechart'], callback: cookPie});

        return userChart;
    }

    function body(as) {
        as = as || {};
   
        var user = as.user || defaultUser;
   
        var userChart = chart(user);
   
        return E.div({'class': 'container_12'}, [
            E.div({id: 'call-to-action', 'class': 'grid_12'}, [
                E.div({'class': 'widgetContent'}, [user.description || defaultUser.description])
            ]),
            
            E.div({'class': 'grid_3 separator'}, [
                E.div({'class': 'widget'}, [
                    E.div({'class': 'widgetContent'}, [
                        E.h3([user.firstName + ' ' + user.lastName]),
                        E.img({src: user.imageUrl, height: '175px', width: '150px'})
                    ]),
        
                    E.div({'class': 'widgetContent'}, [
                        E.h3([user.firstName + " has donated $" + user.dollarsDonated])
                    ])
                ])
            ]),
        
            E.div({'class': 'grid_6 separator'}, [
                E.div({'class': 'widget'}, [
                    E.div({'class': 'widgetContent', style: {textAlign: 'center'}}, [userChart, alignButton(user)]) 
                ]) 
            ]),
        
            E.div({'class': 'grid_3 omega separator'}, [
                E.div({'class': 'widget'}, [
                    E.div({'class': 'widgetContent'}, [
                        E.h3(['Aligned with ' + user.firstName]),
                        E.img({src: user.alignedImageUrl || defaultUser.alignedImageUrl, height: '170px', width: '170px'})
                    ]),
        
                    E.div({'class': 'widgetContent'}, [
                        E.h3([user.firstName + "'s friends have raised $" + user.alignedDonated])
                    ])
                ])
            ]),
        
            E.div({'class': 'grid_12 clear bread'})
        ]);
    }

    function alignButton(user) {
        user = user || defaultUser;
        var alignLink = CORE.button({href: '#'}, ['Donate!']);
        alignLink.onclick = function(e) { 
            //TODO: On click, navigate to appropriate pages
            wait.load({
                buttons: {
                    "Sign In" : function(e) { window.location ="/signup/"; },
                    "Keep Adding Stuff" : function(e) { wait.close(); } 
                }, 
                title: "What do you want to do?",
                content: "From here, you can either sign up to fund your distribution, or continue selecting organizations you would like to support."
            });
        };
        var alignDiv = E.div([
           E.link({type: "text/css", href: "/css/smoothness/jquery-ui-1.8.16.custom.css", rel: "stylesheet"}),
           alignLink
        ]);
        return alignDiv;
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
                var row = L.spoon([
                    L.hug([
                        CORE.h4(user.funds[i]),
                        alignButton(user)
                    ], 30),
                    L.hug([L.pillow(30), fundContents(user.distribution, user.funds[i])])
                ], 25);
                rows.push(row);
            }
            return L.spoon(rows);
        } else {
            return alignButton(user);
        }
    }

    function summary(as) {
        as = as || {};
        var user = as.user || defaultUser;
        var userChart = L.spoon([
            // TODO: On hover, show "dollars raised vs dollars donated"
            L.hug([L.pillow(230, 0), CORE.h1('$' + user.alignedDonated + ' / $' + user.dollarsDonated)]),
            chart(user),
            L.hug([L.pillow(50), distributionTable(user)]),
            L.pillow(20)
        ]);

        return L.spoon([
            CORE.h2(as.title), 
            L.hug([
                L.spoon([
                    L.pillow(20),
                    E.img({style: {width: '175px', height: '225px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
                    CORE.h3([user.firstName + ' ' + user.lastName])
                ], 20),
	        userChart
            ])
        ]);
    }


    function TomBrown(params, nodeReady) {
        require(['tom.json'], function(tom) {
            nodeReady( NAV.frame([body({user: tom})]) );
        });
    }

    function main() {
        return NAV.frame([
            body()
        ]);
    }
   
    define({
        title: "IContrib - Improve the world today",
        main: main,
        body: body,
        summary: summary,
        chart: chart,
        alignButton: alignButton,
        TomBrown: TomBrown
    });
}

require(deps, onReady);

