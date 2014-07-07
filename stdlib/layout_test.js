//
// Interface tests
//

var deps = [
    'layout.js',
    'assert.js'
];

function onReady(layout, assert) {
    var eq = assert.assertEq;
    var gap = layout.gap;
    var hcat = layout.hcat;
    var vcat = layout.vcat;

    //
    // gap
    //

    eq(gap(2).style,   {width: '2px', height: '2px'});


    //
    // hcat
    //

    // To concatenate zero elements will still create a div.
    eq(hcat([]).name, 'div');

    // hcat modifies its input elements
    var g1 = gap(1);
    var g2 = gap(2);
    hcat([g1, g2]);
    eq(g1.style.cssFloat, 'left');
    eq(g1.style.clear, 'none');
    eq(g2.style.cssFloat, 'left');
    eq(g2.style.clear, 'none');


    //
    // vcat
    //

    // vcat modifies its input elements
    g1 = gap(1);
    g2 = gap(2);
    vcat([g1, g2]);
    eq(g1.style.cssFloat, 'left');
    eq(g1.style.clear, 'both');
    eq(g2.style.cssFloat, 'left');
    eq(g2.style.clear, 'both');

    // vcat can right-align elements
    g1 = gap(1);
    g2 = gap(2);
    vcat({align: 'right'}, [g1, g2]);
    eq(g1.style.cssFloat, 'right');
    eq(g1.style.clear, 'both');
    eq(g2.style.cssFloat, 'right');
    eq(g2.style.clear, 'both');

    yoink.define('passed!');
}

yoink.require(deps, onReady);

