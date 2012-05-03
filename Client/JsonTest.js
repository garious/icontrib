//
// This is a script for viewing .json files
//
// Example:  http://localhost/JsonTest?url=/charity/popular.json

var deps = [
    Yoink.params.url
];

function onReady(Data) {
    // Print to console
    console.log(Data);

    // Pretty-print to console
    var s = JSON.stringify(Data, null, 4);
    console.log(s);

    Yoink.define('check your javascript console');
}

Yoink.require(deps, onReady);

