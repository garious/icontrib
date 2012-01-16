var deps = [
    '../tag/tag.js',
    '../tag/layout.js',
    '../css/colors.json',
    '../jquery/jquery-mod.js'
];

function onReady(E, L, C, $) {

    function a(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = {};
        }
        as.style = as.style || {};
        as.style.textDecoration = 'none';
        return E.a(as, xs);
    }

    function button(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        var e = E.a({href: as.href, style: {textDecoration: 'none', backgroundColor: C.middleColor, color: '#fff', borderRadius: '5px', padding: '10px'}}, xs);
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
        as.style.backgroundColor = 'white';
        as.style.border = '2px solid silver';
        as.style.borderRadius = '5px';
  
        var space = L.pillow(15);
        return L.spoon(as, [space, L.hug([space, E.div(xs), space]), space]);
    }

    var font = "15px/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";
    function hStyle(sizeOffset) {
        return {
            font: font,
            fontSize: C.smallestHeader + sizeOffset,
            margin: 0
        };
    }
    function label(s) {
        return E.label({style: {font: font}}, s);
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
        return E.h6({style: h6Style(0)}, s);
    }

    return {
         a: a,
         label: label,
         button: button,
         box: box,
         h1: h1,
         h2: h2,
         h3: h3,
         h4: h4,
         h5: h5,
         h6: h6
    };
}

define(deps, onReady);

