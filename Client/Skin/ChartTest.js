var deps = [
    'Chart.js',
    '/yoink/observable.js'
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

    var chart = chart.pie({
        distribution: dist
    });

    yoink.define(chart);

}

yoink.require(deps, onReady);

