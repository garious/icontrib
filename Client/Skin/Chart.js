var deps = [
    '/Tag/Interface.js',
    '/Tag/ToDom.js',
    '/Tag/TwoDimensional.js',
    '/Tag/Tag.js',
    '/Tag/Observable.js',
    'Colors.js'
];

function onReady(Iface, ToDom, TwoDim, Tag, Observable, Colors) {


    // Uses HTML's canvas element to generate an interactive pie chart
    function pie(as, inputs) {
        return {
            attributes: as,
            inputs: inputs,
            constructor: pie
        };
    }

    var defaultColors = Colors.pieColors;

    pie.interfaces = {};

    var canvasPie_ToDom = {
        toDom: function (me) {
            var as = me.attributes;
            var inputs = me.inputs;

            var e = Tag.tag('div', {style: {width: as.width + 'px', height: as.height + 'px'}});
            var methods = Iface.getInterface(e, ToDom.toDomId);
            var div = methods.toDom(e);

            function draw() {
                 var e = pieSnapshot({distribution: as.distribution, width: as.width, height: as.height, padding: as.padding, colors: as.colors});
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

    pie.interfaces[ToDom.toDomId] = canvasPie_ToDom;

    //pie.interfaces[TwoDim.twoDimensionalId] = {
    //    getDimensions: function (me) {
    //        return {
    //             width: 300,
    //             height: 225
    //        };
    //    },
    //    setPosition: function (me, pos) {
    //        // TODO
    //    }
    //};

    function pieSnapshot(as) {

        var total = 0;
        for (var n = 0; n < as.distribution.length; n += 1) {
             total += as.distribution[n].shares;
        }

        var pcts = as.distribution.map(function(x) {return x.shares / total;});

        var colors = as.colors || defaultColors;
        var padding = as.padding !== undefined ? as.padding : 10;
            
        var r = as.height ? Math.floor(as.height / 2 - padding) : 60;
        var startAngle = -Math.PI / 2;

        var width  = as.width  ? as.width : 2 * r + 2 * padding;
        var height = as.height ? as.height : 2 * r + 2 * padding;
        
        var canvas = document.createElement('canvas');
        if (!canvas.getContext){
            alert('Sorry, but to view this website, you need a web browser that supports the "canvas" element.');
        } else {

            canvas.height = height;
            canvas.width  = width;

            var ctx = canvas.getContext('2d');
            ctx.lineWidth = 3;
            ctx.lineJoin = 'miter';
            ctx.strokeStyle = 'white';

            var x = r + padding;
            var y = r + padding;

            for (var i = 0; i < pcts.length; i += 1) {
            
                var endAngle = startAngle + 2 * Math.PI * pcts[i];

                // Draw pie slice
                ctx.beginPath();
                ctx.moveTo(x, y);
                ctx.arc(x, y, r, startAngle, endAngle, false);
                var color = colors[i % colors.length];
                ctx.fillStyle = color;
                ctx.fill();

                // Draw white mitered lines between slices
                ctx.beginPath();
                ctx.moveTo(x + r * Math.cos(startAngle), y + r * Math.sin(startAngle));
                ctx.lineTo(x, y);
                ctx.lineTo(x + r * Math.cos(endAngle), y + r * Math.sin(endAngle));
                ctx.stroke();
            
                startAngle = endAngle;
            }
        }

        return Tag.tag('div', {style: {width: width + 'px', height: height + 'px', display: 'inline-block'}}, [canvas]);
        
    }

    define({
        pie: pie,
        pieSnapshot: pieSnapshot
    });
}

require(deps, onReady);


