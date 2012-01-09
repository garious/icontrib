// TODO: Generalize this function and move google's jsapi into a module
function exportGoogle(text, yoink, callback) {
    YOINK.interpreters.js(text + '\nreturn google;', yoink, callback);
};

var deps = [
    '../tag/tag.js', 
    '../nav/index.js', 
    {path: '/mirror/google/jsapi', interpreter: exportGoogle},
];

var defaultUser = {
   firstName: 'Greg',
   lastName: 'Fitzgerald',
   description: 'Align with me to support underwater hockey in the United States!',
   imageUrl: '../images/gregf.jpg',
   dollarsDonated: 2456,
   alignedDonated: 12456,
   alignedImageUrl: '../images/friends.png',
   distribution: [
       ['UNICEF', 35],
       ['American Red Cross', 10],
       ['La Jolla Playhouse', 10],
       ['San Diego Foundation', 10], 
       ['USA UWH', 80],
       ['LACC', 20],
   ],
};

function onReady(E, NAV, google) { 

    function body(as) {
        as = as || {};
   
        var user = as.user || defaultUser;
   
        var userChart = E.div({id: 'userChart'}, [
            E.div({id: 'chartPlaceHolder', style: 'width: 400px; height: 300px;'}, [
                E.img({src: '../images/ajax-loader.gif', alt: 'Loading...', style: 'margine: 0px auto;'}),
            ]),
        ]);
   
        var cookPie = function() {
            var options = {title: user.firstName + "'s General Fund", width: 400, height: 300, backgroundColor: { fill:'transparent' }};
            var chart = new google.visualization.PieChart(userChart);
            var data = new google.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            data.addRows(user.distribution);
            chart.draw(data, options);
        };
        google.load('visualization', '1.0', {packages:['corechart'], callback: cookPie});
   
        return E.div({id: 'content', class: 'container_12'}, [
            E.link({type: "text/css", href: "../css/960.css", rel: "stylesheet"}),
            E.link({type: "text/css", href: "../css/main.css", rel: "stylesheet"}),
            E.div({id: 'call-to-action', class: 'grid_12'}, [
                E.div({class: 'widgetContent'}, [user.description || defaultUser.description]),
            ]),
            
            E.div({class: 'grid_3'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + ' ' + user.lastName]),
                        E.img({src: user.imageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + " has donated $" + user.dollarsDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_6'}, [
				E.div({class: 'widget'}, [
					E.div({class: 'widgetContent'}, [
						userChart,
					])
				])
                ]),
            ]),
        
            E.div({class: 'grid_3 omega'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3(['Aligned with ' + user.firstName]),
                        E.img({src: user.alignedImageUrl || defaultUser.alignedImageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + "'s friends have raised $" + user.alignedDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_12 clear bread'}),
        ]);
    };

    function summary(as) {
        var as = as || {};
        var user = as.user || defaultUser;
        return E.div([
            E.link({type: "text/css", href: "../css/960.css", rel: "stylesheet"}),
            E.link({type: "text/css", href: "../css/main.css", rel: "stylesheet"}),
            E.div({class: 'grid_8 widget'}, [
                E.div({class: 'widgetContent'}, [
                    E.h2([as.title || '']),
                    E.div({class: 'influential-box'}, [
                        E.a({href: baseUrl + '/?main='+ user.firstName + user.lastName}, [
                            E.div({class: 'photo'}, [
                                E.img({src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
                            ]),
                        ]),
                        E.h3({class: 'name'}, [user.firstName + ' ' + user.lastName]),
                        E.h4(['Helped raise $' + user.alignedDonated]),
                        E.div({class: 'desc'}, [user.description || '']),
                        E.div([
                            E.a({href: '#'}, ['Align With Me']),
                            E.a({href: '#'}, ['See Other Influential Donors']),
                        ]),
                    ]),
                ]),
            ]),
        ]);
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
        title: "IContrib - Improve the world today.",
        main: main,
        body: body,
        summary: summary,
        TomBrown: TomBrown,
    };
};

return {
    deps: deps,
    callback: onReady,
};

