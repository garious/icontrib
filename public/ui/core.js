var deps = [
    '../tag/tag.js',
    '../css/colors.json',
    '../jquery/jquery-mod.js'
];

function onReady(E, C, $) {

    function button(as, xs) {
        if (xs === undefined) {
            xs = as;
            as = null;
        }
        var e = E.a({href: as.href, style: {backgroundColor: C.middleColor, color: '#fff', borderRadius: '5px', padding: '10px'}}, xs);
        $(e).hover(
            function() {
                e.style.backgroundColor = C.lightColor;
                e.style.textDecoration = 'none';
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
        as.style.backgroundColor = C.lightClearColor;
        as.style.border = '2px solid silver';
        as.style.borderRadius = '5px';

        return E.div(as, [
            E.div({style: {margin: '15px 15px 15px 15px'}}, xs)
        ]);
    }

    function hStyle(sizeOffset) {
        return {
            font: "15px/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif",
            fontWeight: 'normal',
            fontSize: C.smallestHeader + sizeOffset
        };
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

return {
    deps: deps,
    callback: onReady
};
