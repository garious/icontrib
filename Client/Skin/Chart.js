function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/Tag/Interface.js',
    '/Tag/ToDom.js',
    '/Tag/Tag.js',
    'Colors.js',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(Iface, ToDom, Tag, Colors, Google) {


    // Uses Google visualization library to generate an interactive pie chart
    function pie(as) {
        var userChart = Tag.div({style: {width: '300px', height: '225px'}}, [
            Tag.img({src: baseUrl + '/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
        ]);

        var iface = Iface.getInterface(userChart, ToDom.toDomId);
        var chartDiv = iface.toDom(userChart);
   
        var me = {
            element: chartDiv,
            distribution: as.distribution
        };

        var chart = null;
        var data = null;
        var options = {width: 300, height: 225, tooltip: {trigger: 'none'}, legend: {position: 'none'}};
        options.colors = [Colors.darkColor, Colors.middleColor, Colors.lightColor];

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
            chart = new Google.visualization.PieChart(chartDiv);
            data = new Google.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            draw();
        }

        Google.load('visualization', '1.0', {packages:['corechart'], callback: createChart});

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

        return Tag.img({
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


