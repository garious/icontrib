var deps = [
    '/tag/tag.js',
    '/tag/layout1.js',
    'colors.js'
];

function clone(o1) {
    var o2 = {};
    var k;
    for (k in o1) {
        if (o1.hasOwnProperty(k)) {
            o2[k] = o1[k];
        }
    }
    return o2;
}

function onReady(Tag, Layout, Colors) {

    var defaultFont = "/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";
    var defaultFontSize = 15;
    var font = defaultFontSize + "px" + defaultFont;

    function textDimensions(as, s) {
        var canvas = Tag.canvas();
        var fontSize = as.fontSize || defaultFontSize;

        if (canvas && canvas.getContext) {

            var ctx = canvas.getContext('2d');
            ctx.font = fontSize + "px" + defaultFont;
            ctx.fontSize = fontSize;

            var dim = ctx.measureText(s);

            return {
                width: dim.width,
                height: fontSize + 6
            };

        } else {
            // No canvas available on this browser - time to guess.
            return {
                width: fontSize * s.length * 0.6,
                height: fontSize + 6
            };
        }
    }

    function a(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        as = as && clone(as) || {};
        as.style = as.style && clone(as.style) || {};

        var s = typeof xs === 'string' && xs || xs[0];
        var dim = textDimensions({}, s);

        as.style.textDecoration = 'none';
        as.style.font = font;
        as.style.width = dim.width + 'px';
        as.style.height = dim.height + 'px';
        as.style.color = 'blue';

        var e = Tag.a(as, xs);
        e.addEventListener('mouseover', function() {
            e.style.textDecoration = 'underline';
        });
        e.addEventListener('mouseout', function() {
            e.style.textDecoration = 'none';
        });
        return e;
    }

    function input(as) {
        var e = Tag.input(as);
        e.style.width  = as.width  || e.size * 10 + 'px';
        e.style.height = as.height || 20;
        return e;
    }

    function button(as) {
        var dim = textDimensions({}, as.text);

        var color      = as.loud ? Colors.red : Colors.middleColor;
        var focusColor = as.loud ? Colors.red : Colors.lightColor;

        var e = Tag.a({
            href: as.href || '#', 
            style: {
                width: dim.width + 'px',
                height: dim.height + 'px',
                font: font, 
                textDecoration: 'none', 
                textAlign: 'center', 
                backgroundColor: color, 
                color: '#fff', 
                padding: '5px', 
                borderRadius: '2px'
            }
        }, as.text);

        e.addEventListener('mouseover', function() {
            e.style.backgroundColor = focusColor;
        });
        e.addEventListener('mouseout', function() {
            e.style.backgroundColor = color;
        });

        return e;
    }

    function box(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null; 
        }
        
        as = as || {};
        xs = xs || [];

        var shadow = '0px 0px 5px 2px #ddd';
        var space = Layout.pillow(15);

        var e = Layout.spoon([
            space, 
            Layout.hug([space].concat(xs).concat([space])),
            space
        ]);

        return Tag.div({
            style: {
                border: '2px solid #cfcfcf',
                shadow: shadow,
                MozBoxShadow: shadow,
                WebkitBoxShadow: shadow,
                width:  as.width  || e.style.width,
                height: as.height || e.style.height
            }
        }, [e]);
    }

    // Create the style attribute for HTML header elements
    function hStyle(fontSize, s) {
        var dim = textDimensions({fontSize: fontSize}, s);

        return {
            width: dim.width,
            height: dim.height,
            font: font,
            fontSize: fontSize,
            margin: 0,
            color: Colors.darkColor
        };
    }

    // Create a header constructor.  
    //
    // The returned constructor accepts either an attributes object or a string.
    //
    //     mkHeader(2)('hello!') === h2('hello!')
    //     mkHeader(3)('hello!') === h3('hello!')
    //     mkHeader(3)('hello!') === h3({text: 'hello!'})
    //
    function mkHeader(n) {

        function header(as) {
            var s = typeof as === "string" ? as : as.text;

            var sty = hStyle(Colors['h' + n + 'Size'], s); 

            if (typeof as === "object") {
                sty.color = as.color !== 'undefined' ? as.color : sty.color;
            }

            return Tag['h' + n]({style: sty}, s);
        }

        return header;
    }

    function label(s) {
        var dim = textDimensions({}, s);
        return Tag.label({style: {width: dim.width, height: dim.height, font: font}}, s);
    }

    function p(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        as = as && clone(as) || {};
        as.style = as.style && clone(as.style) || {};

        as.style.font = font;

        return Tag.p(as, xs);
    }

    function hr(as) {
        as = as || {};

        var sty = {
           height: as.height ? as.height + 'px' : 0,
           width:  as.width  ? as.width  + 'px' : '100%',
           margin: 0
        };

        return Tag.hr({style: sty});
    }

    define({
         a: a,
         input: input,
         label: label,
         button: button,
         box: box,
         h1: mkHeader(1),
         h2: mkHeader(2),
         h3: mkHeader(3),
         h4: mkHeader(4),
         h5: mkHeader(5),
         h6: mkHeader(6),
         p: p,
         hr: hr 
    });
}

require(deps, onReady);

