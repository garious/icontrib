
var deps = [
    '/Tag/Observable.js',
    '/Tag/Tag.js',
    '/Tag/Layout.js',
    '/Tag/Shapes.js',
    'Colors.js'
];

function onReady(Observable, Tag, Layout, Shapes, Colors) {

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

        return Tag.tag('hr', {style: sty, noshade: true, size: 1});
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

        function calcLeftWidth(v) {
            var w = (width * v / 100) - radius;
            return (w > 0 ? w : 0) + 'px'; 
        }

        function calcRightWidth(v) {
            var w = (width * (100 - v) / 100) - radius;
            return (w > 0 ? w : 0) + 'px'; 
        }

        var val = as.value;

        function onMouseDown(evt) {

            // First, track down the element that we attached the event handler to
            var e = evt.target;
            while (e && e.constructor !== HTMLDivElement) {
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

        var circle = Shapes.circle({radius: radius, color: as.color, 'top': as.marginTop});

        // Create an observable 'leftWidth' that holds the value returned by calcLeftWidth any time 'val' changes.
        var leftWidth  = Observable.thunk([val], calcLeftWidth);
        var rightWidth = Observable.thunk([val], calcRightWidth);

        var lineMarginTop = as.marginTop + radius - height / 2;

        var lines = Layout.hug([
            line({width:  leftWidth, height: height, marginTop: lineMarginTop, marginBottom: as.marginBottom, color: as.color}),
            circle,
            line({width: rightWidth, height: height, marginTop: lineMarginTop, marginBottom: as.marginBottom, color: Colors.gray})
        ]);
        return Tag.tag('div', [lines], {mousedown: onMouseDown});
    }

    define({
        slider: slider
    });
}

require(deps, onReady);

