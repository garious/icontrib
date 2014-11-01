
var deps = [
    '/stdlib/observable.js',
    '/stdlib/dom.js',
    '/stdlib/layout.js',
    'shapes.js',
    'colors.js'
];

function onReady(observable, dom, layout, shapes, colors) {

    function line(as) {
        as = as || {};

        var sty = {
           height: as.height ? as.height + 'px' : '1px',
           width:  as.width,
           margin: as.margin ? as.margin + 'px' : 0,
           marginLeft:   as.marginLeft   ? as.marginLeft   + 'px' : 0,
           marginRight:  as.marginRight  ? as.marginRight  + 'px' : 0,
           marginTop:    as.marginTop    ? as.marginTop    + 'px' : 0,
           marginBottom: as.marginBottom ? as.marginBottom + 'px' : 0,
           borderWidth: 0,
           backgroundColor: as.color
        };

        return dom.element({name: 'hr', style: sty, attributes: {noshade: true, size: 1}});
    }

    // Get the X coordinate of an element, relative to the page.
    function getPageX(e) {

        var x = 0;
        do {
           x += e.offsetLeft;
           e = e.offsetParent;
        } while (e);

        return x;
    }

    // Create a slider element
    function slider(as) {
        var width = as.width || 200;
        var height = as.height || 4;
        var radius = 8;
        var maxLineWidth = width - 2 * radius;

        function calcLeftWidth(v) {
            var w = (width * v / 100) - radius;
            return (w >= 0 ? (w >= maxLineWidth ? maxLineWidth : w) : 0) + 'px'; 
        }

        function calcRightWidth(v) {
            var w = (width * (100 - v) / 100) - radius;
            return (w >= 0 ? (w >= maxLineWidth ? maxLineWidth : w) : 0) + 'px'; 
        }

        var val = as.value;
        var mouseDown = false;

        function onMouseDown(evt) {
            mouseDown = true;
            onMouseMove(evt);
        }

        function onMouseMove(evt) {
            if (mouseDown) {
                // First, track down the element that we attached the event handler to
                var e = evt.target;
                while (e && !e.dataset.slider) {
                    e = e.parentNode;
                }

                if (e) {
                    var divOffset = evt.pageX - getPageX(e);

                    if (divOffset > 0 && divOffset < width) {
                        var v = divOffset / width * 100;
                        as.onChange({target: {value: v}});
                        //val.set(divOffset / width * 100);
                    } else {
                        console.error('onMouseDown: event out of bounds: ', divOffset, width);
                    }
                } else {
                    console.error('onMouseDown: expected parent node to be a DIV element', evt.target);
                }
            }
        }

        function onMouseUp(evt) {
            if (evt.target.dataset.slider) {
                mouseDown = false;
            }
        }

        // Create an observable 'leftWidth' that holds the value returned by calcLeftWidth any time 'val' changes.
        var leftWidth  = observable.thunk([val], calcLeftWidth);
        var rightWidth = observable.thunk([val], calcRightWidth);

        var lineMarginTop = as.marginTop + radius - height / 2;

        var lines = layout.hcat([
            layout.gap(radius),
            line({width:  leftWidth, height: height, marginTop: lineMarginTop, marginBottom: lineMarginTop, color: as.color}),
            line({width: rightWidth, height: height, marginTop: lineMarginTop, marginBottom: lineMarginTop, color: colors.gray}),
            layout.gap(radius)
        ]);

        var circle = shapes.circle({left: leftWidth, radius: radius, color: as.color, 'top': as.marginTop + 'px'});

        var handlers = {mousedown: onMouseDown, mousemove: onMouseMove, mouseup: onMouseUp, mouseout: onMouseUp};
        return dom.element({
            name: 'div',
            attributes: {'data-slider': true},
            style: {width: width + 'px'},
            contents: [lines, circle],
            handlers: handlers
        });
    }

    define({
        slider: slider
    });
}

require(deps, onReady);

