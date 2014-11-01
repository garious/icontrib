var deps = [
    'widgets.js',
    '/stdlib/observable.js'
];

function increment(x) {
    return String(parseInt(x, 10) + 1);
}

function onReady(widgets, observable) {
    var o = observable.publisher("0");
    var oIncrement = observable.lift(increment);
    var box = widgets.box([
        widgets.numInput(o),
        widgets.numInput(oIncrement(o), true)
    ]);
    define(box);
}

require(deps, onReady);

