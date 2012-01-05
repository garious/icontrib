//
// This file will always expose the latest jQuery as a Yoink module
//

function exportJQuery(text, yoink, callback) {
    YOINK.interpreters.js(text + '\nreturn jQuery;', yoink, callback);
};


var deps = [
    {path: 'jquery-1.7.1.min.js', interpreter: exportJQuery},
];

function callback(JQUERY) {
    return JQUERY;
}

return YOINK.module(deps, callback);

