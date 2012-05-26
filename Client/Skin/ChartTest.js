var deps = [
    'Chart.js',
    '/Tag/Observable.js'
];

function onReady(Chart, Observable) {

    var dist = [
        Observable.observe(5),
        Observable.observe(95)
    ];

    // TODO: support this variation instead
    //var dist = Observable.observe([
    //    5,
    //    95
    //]);

    var chart = Chart.pie({
        distribution: dist
    });

    Yoink.define(chart);

}

Yoink.require(deps, onReady);

