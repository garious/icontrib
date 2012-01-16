// TODO: Generalize this function and move google's jsapi into a module
function exportGoogle(text, yoink, callback) {
    YOINK.interpreters.js(text + '\nreturn google;', yoink, callback);
}

var deps = [
    '../tag/tag.js', 
    '../tag/layout.js',
    '../ui/nav.js', 
    {path: '/mirror/google/jsapi', interpreter: exportGoogle},
    '/widgets/waitScreen.js',
    '../ui/core.js',
    '../ui/colors.json'
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
            data.addRows(user.distribution);
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
        var alignLink = CORE.button({href: '#'}, ['Align with ' + user.firstName]);
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

    function summary(as) {
        as = as || {};
        var user = as.user || defaultUser;
        var userChart = L.spoon([
            chart(user),
            L.hug([L.pillow(100), alignButton(user)])
        ]);

        return L.spoon([
            CORE.h2(as.title),
            L.hug([
                L.spoon([
                    L.pillow(20),
                    E.img({style: {width: '175px', height: '225px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
                    CORE.h3([user.firstName + ' ' + user.lastName]),
                    CORE.h4(['Helped raise $' + user.alignedDonated])
                ], 20),
	        userChart
            ])
        ]);
    }

    function charity(as) {
        as = as || {};
        var user = as.user || defaultUser;
	var box = CORE.box([
                L.spoon([
	            CORE.h2(user.firstName),
                    L.hug([
                        L.spoon([
                            E.img({style: {width: '175px', height: '175px', borderRadius: '5px'}, src: user.imageUrl, alt: user.firstName})
                        ], 20),
                        L.spoon([
                            E.p({style: {width: '600'}}, user.mission),
                            E.br(),
	                    alignButton(user)
                        ], 20)
                    ], 30)
                ], 20)
            ]);

        return L.hug([
            L.pillow(100),
            box
       ]);
    }

    function Usoa() {
        return {
            deps: ['usoa.json'], 
            callback: function(u) {
                return NAV.frame([
                    charity({user: u})
                ]);
            }
        };
    }

    function GlobalFundForWomen() {
        return {
            deps: ['gffw.json'], 
            callback: function(u) {
                return NAV.frame([
                    charity({user: u})
                ]);
            }
        };
    }

    function TomBrown() {
        function tomReady(tom) {
            return main({user: tom});
        }
        return {deps: ['tom.json'], callback: tomReady};
    }

    function main(as) {
        return NAV.frame([
            body(as)
        ]);
    }
   
    return {
        title: "IContrib - Improve the world today",
        main: main,
        body: body,
        summary: summary,
        charity: charity,
        chart: chart,
        alignButton: alignButton,
        TomBrown: TomBrown,
        Usoa: Usoa,
        GlobalFundForWomen: GlobalFundForWomen
    };
}

define(deps, onReady);

