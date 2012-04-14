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
    {path: '/mirror/google/jsapi', interpreter: exportGoogle, onError: function(){return null;}}
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

    var defaultColors = [
        Colors.darkColor,
        Colors.middleColor,
        Colors.lightColor
    ];

    pie.interfaces = {};
    var google_ToDom = {
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
            options.colors = as.colors || defaultColors;

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

    var canvasPie_ToDom = {
        toDom: function (me) {
            var as = me.attributes;
            var inputs = me.inputs;

            var e = Tag.div({style: {width: as.width + 'px', height: as.height + 'px'}});
            var methods = Iface.getInterface(e, ToDom.toDomId);
            var div = methods.toDom(e);

            function draw() {
                 var e = pie2({distribution: as.distribution, width: as.width, height: as.height});
                 var methods = Iface.getInterface(e, ToDom.toDomId);
                 div.innerHTML = '';
                 div.appendChild( methods.toDom(e) );
            }

            for (var i = 0; i < inputs.length; i++) {
                var obsMethods = Iface.getInterface(inputs[i], Observable.observableId);
                obsMethods.subscribe(inputs[i], draw);
            }

            draw();

            return div;
        }
    };

    pie.interfaces[ToDom.toDomId] = Google ? google_ToDom : canvasPie_ToDom;

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

    function pie2(as) {

        var total = 0;
        for (var n = 0; n < as.distribution.length; n += 1) {
             total += as.distribution[n].shares;
        }

        var pcts = as.distribution.map(function(x) {return x.shares / total;});

        var colors = as.colors || defaultColors;
        var padding = as.padding !== undefined ? as.padding : 10;
            
        var r = as.height ? Math.floor(as.height / 2 - padding) : 60;
        var startAngle = -Math.PI / 2;
        
        var canvas = document.createElement('canvas');
        if (!canvas.getContext){
            alert('Sorry, but to see this demo, you need a web browser that supports the "canvas" element.');
        } else {

            canvas.height = 2 * r + 2 * padding;
            canvas.width  = 2 * r + 2 * padding;
            canvas.style.position = 'absolute';

            var ctx = canvas.getContext('2d');
            ctx.lineWidth = 3;
            ctx.lineCap = 'round';
            ctx.strokeStyle = 'white';

            var x = r + padding;
            var y = r + padding;

            for (var i = 0; i < pcts.length; i += 1) {
            
                var angle = 2 * Math.PI * pcts[i];
                ctx.beginPath();

                // Draw pie slice
                ctx.moveTo(x, y);
                ctx.arc(x, y, r, startAngle, startAngle + angle, false);
                ctx.lineTo(x, y);

                var color = colors[i % colors.length];
                ctx.fillStyle = color;
                ctx.fill();

                // Draw border
                ctx.stroke();
            
                startAngle += angle;
            }
        }

        var width  = (as.width  ? as.width : 150) + 'px';
        var height = (as.height ? as.height : 150) + 'px';
        return Tag.tag('div', {style: {width: width, height: height}}, [canvas]);
        
    }

    define({
        pie: pie,
        pie1: pie1,
        pie2: pie2
    });
}

require(deps, onReady);


