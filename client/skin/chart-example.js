var chart = require('./chart');
var observable = require('poochie/observable');

var dist = [
    observable.observe(5),
    observable.observe(95)
];

// TODO: support this variation instead
//var dist = observable.observe([
//    5,
//    95
//]);

module.exports = chart.pie({
    distribution: dist
});

