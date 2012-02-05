var deps = [
    '/tag/tag.js',
    '/tag/layout.js',
    'colors.json',
    '/jquery/jquery-mod.js'
];

function onReady(E, L, C, $) {

    var font = "15px/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";

    function a(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = {};
        }
        as = as || {};
        as.style = as.style || {};
        as.style.textDecoration = 'none';
        as.style.font = font;
        var e = E.a(as, xs);
        $(e).hover(
            function() {
                e.style.textDecoration = 'underline';
            },
            function() {
                e.style.textDecoration = 'none';
            }
        );
        return e;
    }

    function button(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = {};
        }
        var e = E.a({href: as.href || '#', style: {font: font, textDecoration: 'none', backgroundColor: C.middleColor, color: '#fff', padding: '5px', borderRadius: '2px'}}, xs);
        $(e).hover(
            function() {
                e.style.backgroundColor = C.lightColor;
            },
            function() {
                e.style.backgroundColor = C.middleColor;
            }
        );
        return e;
    }

    function box(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = {};
        }
        as.style = as.style || {};
        as.style.border = '2px solid #cfcfcf';
        as.style.backgroundColor = 'white';

        var shadow = '0px 0px 5px 2px #ddd';
        as.style.MozBoxShadow = shadow;
        as.style.WebkitBoxShadow = shadow;
        as.style.Shadow = shadow;
  
        var space = L.pillow(15);

        return E.div(as, [
            L.spoon([
                space, 
                L.hug([space].concat(xs).concat([space])),
                space
            ])
        ]);
    }

    function hStyle(sizeOffset) {
        return {
            font: font,
            fontSize: C.smallestHeader + sizeOffset,
            margin: 0
        };
    }
    function label(s) {
        return E.label({style: {width: 10 * s.length, height: 20, font: font}}, s);
    }
    function h1(s) {
        return E.h1({style: hStyle(10)}, s);
    }
    function h2(s) {
        return E.h2({style: hStyle(8)}, s);
    }
    function h3(s) {
        return E.h3({style: hStyle(6)}, s);
    }
    function h4(s) {
        return E.h4({style: hStyle(4)}, s);
    }
    function h5(s) {
        return E.h5({style: hStyle(2)}, s);
    }
    function h6(s) {
        return E.h6({style: hStyle(0)}, s);
    }
    function p(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = {};
        }
        as = as || {};
        as.style = as.style || {};
        as.style.font = font;
        return E.p(as, xs);
    }

    define({
         a: a,
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

