function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/tag/tag.js',
    'colors.js',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(E, C, GOOGLE) {


    // Uses Google visualization library to generate an interactive pie chart
    function pie(as) {
        var userChart = E.div({style: {width: '300px', height: '225px'}}, [
            E.img({src: baseUrl + '/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
        ]);
   
        var me = {
            element: userChart,
            distribution: as.distribution
        };

        var chart = null;
        var data = null;
        var options = {width: 300, height: 225, tooltip: {trigger: 'none'}, legend: {position: 'none'}};
        options.colors = [C.darkColor, C.middleColor, C.lightColor];

        function draw() {
            if (data && chart) {
                 var dist = [];
                 for (var i = 0; i < me.distribution.length; i++) {
                     var ud = me.distribution[i]; 
                     dist.push([ud.name, ud.shares]); 
                 }
                 data.removeRows(0, data.getNumberOfRows());
                 data.addRows(dist);
                 chart.draw(data, options);
            }
        }

        function createChart() {
            chart = new GOOGLE.visualization.PieChart(userChart);
            data = new GOOGLE.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            draw();
        }

        GOOGLE.load('visualization', '1.0', {packages:['corechart'], callback: createChart});

        // expose the draw function to the caller
        me.draw = draw;

        return me;
    }

    // Uses Google API to generate a simple pie chart image
    function pie1(as) {
        var height = 150;
        var width = 150;
        var chs = height + 'x' + width;
        var chd = 't:';
        for (var i = 0; i < as.distribution.length; i++) {
            var x = as.distribution[i]; 
            if (i != as.distribution.length - 1) {
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


