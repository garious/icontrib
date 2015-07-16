var observable = require('poochie/observable');
var request = require('browser-request');

// Return a publisher that publishes the given initial value
// until the server replies with data.
function jsonPublisher(url, initialValue) {
    var o = observable.publisher(initialValue);
    request(url, function onResponse(err, resp) {
        if (!err) {
            o.set(resp);
	}
    });
    return o;
}

module.exports = {
    jsonPublisher: jsonPublisher
};
