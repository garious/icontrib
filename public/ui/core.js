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

    function box(xs) {
        return E.div({'class': 'widget'}, [
            E.div({'class': 'widgetContent'}, xs)
        ]);
    }

    return {
         button: button,
         box: box
    };
}

return {
    deps: deps,
    callback: onReady
};
