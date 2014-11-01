var deps = [
    'chart.js',
    '/stdlib/observable.js'
];

function onReady(chart, observable) {

    var dist = [
        observable.observe(5),
        observable.observe(95)
    ];

    // TODO: support this variation instead
    //var dist = observable.observe([
    //    5,
    //    95
    //]);

    var pie = chart.pie({
        distribution: dist
    });

    define(pie);

}

require(deps, onReady);

