//
// This is a script for viewing .json files
//
// Example:  http://localhost/json-test?url=/charity/popular.json

var deps = [
    yoink.params.url
];

function onReady(Data) {
    // Print to console
    console.log(Data);

    // Pretty-print to console
    var s = JSON.stringify(Data, null, 4);
    console.log(s);

    define('check your javascript console');
}

require(deps, onReady);

