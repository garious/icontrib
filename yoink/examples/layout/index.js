//
// 2D Layout tests
//

var deps = [
    '/stdlib/dom.js',
    '/stdlib/layout.js'
];

function onReady(dom, layout) {

    var gap10 = layout.gap(10);

    var testImg = dom.element({
        name: 'img',
        attributes: {src: 'logo.png'},
        style: {border: '1px solid', padding: '5px', borderRadius: '5px'}
    });

    var separator = layout.gap(30);

    function label(s, e) {
        return layout.hcat([dom.element({name: 'p', style: {width: '70px'}, contents: s}), gap10, e]);
    }

    var images = [testImg, gap10, testImg, gap10, testImg];
    var div = layout.hcat([
        gap10,
        layout.vcat([
            gap10,
            layout.vcat([
                label('hcat', layout.hcat(images)), separator,
                label('vcat', layout.vcat(images)), separator,
            ]),
            gap10
        ]),
        gap10
    ]);

    define(div);
}


require(deps, onReady);

