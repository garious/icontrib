//
// This is a script for viewing .json files
//
// Example:  http://localhost/json-test?url=/charity/popular.json

var request = require('browser-request');

request(yoink.params.url, function (err, data) {
    // Print to console
    console.log(data);

    if (!err) {
        // Pretty-print to console
        var s = JSON.stringify(data, null, 4);
        console.log(s);
    }
})

module.exports = 'check your javascript console';
