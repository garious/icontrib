//
// This file will always expose the latest jQuery as a module
//

// TODO: Generalize this function
function exportJQuery(text, yoink, callback) {
    YOINK.interpreters.js(text + '\ndefine( jQuery.noConflict(true) );', yoink, callback);
}

var deps = [
    {path: 'jquery-1.7.1.min.js', interpreter: exportJQuery}
];

function identity(x) {
    return x;
}

define(deps, identity);

