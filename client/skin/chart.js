var deps = [
    '/stdlib/dom.js',
    '/stdlib/observable.js',
    'colors.js'
];

function onReady(dom, observable, colors) {


    // Uses HTML's canvas element to generate an interactive pie chart
    function pie(as) {
        return {
            attributes: as,
            constructor: pie,
            render: function () {
                var e = dom.element({
                    name: 'div',
                    style: {width: as.width + 'px', height: as.height + 'px'}
                });

                var div = e.render();

                function draw() {
                     var distSnapshot = observable.snapshot(as.distribution);
                     var e = pieSnapshot({distribution: distSnapshot, width: as.width, height: as.height, padding: as.padding, colors: as.colors});
                     div.innerHTML = '';
                     div.appendChild( e.render() );
                }

                for (var i = 0; i < as.distribution.length; i++) {
                    var obs = as.distribution[i];
		    if (obs instanceof observable.Observable) {
                        obs.subscribe(draw);
                    }
                }

                draw();

                return div;
            }
        };
    }

    var defaultColors = colors.pieColors;

    function pieSnapshot(as) {

        var total = 0;
        var dist = as.distribution;

        for (var n = 0; n < dist.length; n += 1) {
             total += dist[n];
        }

        var pcts = dist.map(function(x) {return x / total;});

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

        return dom.element({
            name: 'div',
            style: {width: width + 'px', height: height + 'px', display: 'inline-block'},
            contents: [canvas]
        });
        
    }

    define({
        pie: pie,
        pieSnapshot: pieSnapshot
    });
}

require(deps, onReady);


