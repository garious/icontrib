//
// Interface tests
//

var deps = [
    'layout.js',
    'assert.js',
    'dom.js'
];

function onReady(layout, assert, dom) {
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

    // hcat's contents is an array
    eq(hcat([]).contents instanceof Array, true);

    // hcat returns a new div for its input elements
    var g1 = gap(1);
    var g2 = gap(2);
    var div = hcat([g1, g2]);
    g1 = div.contents[0];
    g2 = div.contents[1];

    // g1 is an Element
    eq(g1 instanceof dom.Element, true);

    eq(g1.style.cssFloat, 'left');
    eq(g1.style.clear, 'none');
    eq(g2.style.cssFloat, 'left');
    eq(g2.style.clear, 'none');


    //
    // vcat
    //

    // vcat returns a new div containing its input elements
    g1 = gap(1);
    g2 = gap(2);
    div = vcat([g1, g2]);
    g1 = div.contents[0];
    g2 = div.contents[1];
    eq(g1.style.cssFloat, 'left');
    eq(g1.style.clear, 'both');
    eq(g2.style.cssFloat, 'left');
    eq(g2.style.clear, 'both');

    // vcat can right-align elements
    g1 = gap(1);
    g2 = gap(2);
    div = vcat({align: 'right'}, [g1, g2]);
    g1 = div.contents[0];
    g2 = div.contents[1];
    eq(g1.style.cssFloat, 'right');
    eq(g1.style.clear, 'both');
    eq(g2.style.cssFloat, 'right');
    eq(g2.style.clear, 'both');

    define('passed!');
}

require(deps, onReady);

