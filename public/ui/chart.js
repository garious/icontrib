function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/tag/tag.js',
    'colors.json',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(E, C, GOOGLE) {


    // Uses Google visualization library to generate an interactive pie chart
    function pie(user) {
        var dist = [];
        for (var i = 0; i < user.distribution.length; i++) {
            var ud = user.distribution[i]; 
            dist.push([ud.name, ud.shares]); 
        }
        var userChart = E.div({style: {width: '300px', height: '225px'}}, [
            E.img({src: '/images/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
        ]);
   
        var cookPie = function() {
            var options = {width: 300, height: 225, tooltip: {trigger: 'none'}, legend: {position: 'none'}};
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

    // Uses Google API to generate a simple pie chart image
    function pie1(user) {
        var height = 150;
        var width = 150;
        var chs = height + 'x' + width;
        var chd = 't:';
        for (var i = 0; i < user.distribution.length; i++) {
            var x = user.distribution[i]; 
            if (i != user.distribution.length - 1) {
               chd += x.shares + ','; 
            } else {
               chd += x.shares;
            }
        }

        return E.img({
            src: 'https://chart.googleapis.com/chart?cht=p&chco=a7d322&chs=' + chs + '&chd=' + chd,
            alt: 'Chart',
            style: {width: width + 'px', height: height + 'px'}
        });
    }

    define({
        pie: pie,
        pie1: pie1
    });
}

require(deps, onReady);


