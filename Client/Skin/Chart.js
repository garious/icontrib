function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/Tag/Interface.js',
    '/Tag/ToDom.js',
    '/Tag/TwoDimensional.js',
    '/Tag/Tag.js',
    '/Tag/Observable.js',
    'Colors.js',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(Iface, ToDom, TwoDim, Tag, Observable, Colors, Google) {


    // Uses Google visualization library to generate an interactive pie chart
    function pie(as, inputs) {
        return {
            attributes: as,
            inputs: inputs,
            constructor: pie
        };
    }

    pie.interfaces = {};
    pie.interfaces[ToDom.toDomId] = {
        toDom: function (me) {
            var as = me.attributes;
            var inputs = me.inputs;

            var userChart = Tag.div({style: {width: '300px', height: '225px'}}, [
                Tag.img({src: baseUrl + '/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
            ]);

            var iface = Iface.getInterface(userChart, ToDom.toDomId);
            var chartDiv = iface.toDom(userChart);
   
            var chart = null;
            var data = null;
            var options = {width: 300, height: 225, tooltip: {trigger: 'none'}, legend: {position: 'none'}};
            options.colors = [Colors.darkColor, Colors.middleColor, Colors.lightColor];

            function draw() {
                if (data && chart) {
                     var dist = [];
                     for (var i = 0; i < as.distribution.length; i++) {
                         var ud = as.distribution[i]; 
                         var methods = Iface.getInterface(inputs[i], Observable.observableId);
                         dist.push([ud.name, methods.get(inputs[i])]); 
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

                for (var i = 0; i < inputs.length; i++) {
                    var methods = Iface.getInterface(inputs[i], Observable.observableId);
                    methods.subscribe(inputs[i], draw);
                }
            }

            Google.load('visualization', '1.0', {packages:['corechart'], callback: createChart});

            var e = Tag.div({style: {width: '300px', height: '225px'}}, [chartDiv]);
            var methods = Iface.getInterface(e, ToDom.toDomId);
            return methods.toDom(e);
        }
    };

    pie.interfaces[TwoDim.twoDimensionalId] = {
        getDimensions: function (me) {
            return {
                 width: 300,
                 height: 225
            };
        },
        setPosition: function (me, pos) {
            // TODO
        }
    };

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


