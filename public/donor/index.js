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
   imageUrl: '../images/gregf.jpg',
   dollarsDonated: '$2,456',
   alignedDonated: '$12,456',
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

    function body(user) {
   
        user = user || defaultUser;
   
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
            E.div({id: 'call-to-action', class: 'grid_12'}, [
                E.div({class: 'widgetContent'}, [
                    'Align with me to support underwater hockey and to make San Diego more awesome!'
                ]),
            ]),
            
            E.div({class: 'grid_3 alpha'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + ' ' + user.lastName]),
                        E.img({src: user.imageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + "'s donated " + user.dollarsDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_6 widget'}, [
                E.div({class: 'widgetContent'}, [
                    userChart,
                ]),
            ]),
        
            E.div({class: 'grid_3 omega'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3(['Aligned with ' + user.firstName]),
                        E.img({src: user.alignedImageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + "'s friends have raised " + user.alignedDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_12 clear bread'}),
        ]);
    };

    function main() {
        return NAV.frame([
            body()
        ]);
    }
   
    return {
        title: "IContrib - Improve the world today.",
        main: main,
        body: body,
    };
};

return {
    deps: deps,
    callback: onReady,
};

