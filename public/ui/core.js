var deps = [
    '/tag/tag.js',
    '/tag/layout1.js',
    'colors.json'//,
    //'/jquery/jquery-mod.js'
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

function onReady(E, L, C, $) {

    var font = "15px/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";

    function textDimensions(as, s) {
        // TODO: use at least font, fontSize and maxWidth
        var fontSize = as.fontSize || 15;
        return {
            width: fontSize * s.length * 0.6,
            height: fontSize + 6
        };
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

        var e = E.a(as, xs);
        //$(e).hover(
        //    function() {
        //        e.style.textDecoration = 'underline';
        //    },
        //    function() {
        //        e.style.textDecoration = 'none';
        //    }
        //);
        return e;
    }

    function input(as) {
        var e = E.input(as);
        e.style.width  = as.width  || e.size * 10 + 'px';
        e.style.height = as.height || 20;
        return e;
    }

    function button(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        as = as && clone(as) || {};

        var s = typeof xs === 'string' && xs || xs[0];
        var dim = textDimensions({}, s);

        var e = E.a({
            href: as.href || '#', 
            style: {
                width: dim.width + 'px',
                height: dim.height + 'px',
                font: font, 
                textDecoration: 'none', 
                textAlign: 'center', 
                backgroundColor: C.middleColor, 
                color: '#fff', 
                padding: '5px', 
                borderRadius: '2px'
            }
        }, xs);

        //$(e).hover(
        //    function() {
        //        e.style.backgroundColor = C.lightColor;
        //    },
        //    function() {
        //        e.style.backgroundColor = C.middleColor;
        //    }
        //);

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
        var space = L.pillow(15);

        var e = L.spoon([
            space, 
            L.hug([space].concat(xs).concat([space])),
            space
        ]);

        return E.div({
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

    function hStyle(sizeOffset, s) {
        var fontSize = C.smallestHeader + sizeOffset;
        var dim = textDimensions({fontSize: fontSize}, s);

        return {
            width: dim.width,
            height: dim.height,
            font: font,
            fontSize: fontSize,
            margin: 0
        };
    }
    
    function label(s) {
        var dim = textDimensions({}, s);
        return E.label({style: {width: dim.width, height: dim.height, font: font}}, s);
    }
    function h1(s) {
        return E.h1({style: hStyle(10, s)}, s);
    }
    function h2(s) {
        return E.h2({style: hStyle(8, s)}, s);
    }
    function h3(s) {
        return E.h3({style: hStyle(6, s)}, s);
    }
    function h4(s) {
        return E.h4({style: hStyle(4, s)}, s);
    }
    function h5(s) {
        return E.h5({style: hStyle(2, s)}, s);
    }
    function h6(s) {
        return E.h6({style: hStyle(0, s)}, s);
    }
    function p(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        as = as && clone(as) || {};
        as.style = as.style && clone(as.style) || {};

        as.style.font = font;

        return E.p(as, xs);
    }

    define({
         a: a,
         input: input,
         label: label,
         button: button,
         box: box,
         h1: h1,
         h2: h2,
         h3: h3,
         h4: h4,
         h5: h5,
         h6: h6,
         p: p
    });
}

require(deps, onReady);

