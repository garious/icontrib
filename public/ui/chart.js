function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/tag/tag.js',
    'colors.json',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(E, C, GOOGLE) {


    function pie(user) {
        var dist = [];
        for (var i = 0; i < user.distribution.length; i++) {
            var ud = user.distribution[i]; 
            dist.push([ud.name, ud.shares]); 
        }
        var userChart = E.div({style: {width: '400px', height: '300px'}}, [
            E.img({src: '/images/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
        ]);
   
        var cookPie = function() {
            var options = {width: 400, height: 300, backgroundColor: { fill:'transparent' }};
            options.colors = [C.darkColor, C.middleColor, C.lightColor];
            var chart = new GOOGLE.visualization.PieChart(userChart);
            var data = new GOOGLE.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            data.addRows(dist);
            chart.draw(data, options);
        };
        GOOGLE.load('visualization', '1.0', {packages:['corechart'], callback: cookPie});

        return userChart;
    }

    define({
        pie: pie
    });
}

require(deps, onReady);


