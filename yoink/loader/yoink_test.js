//
// yoink.js tests using Node.js 
//
// Usage: node yoink_test.js
//

var fs = require('fs');
var assert = require('assert');

//
// Mock XMLHttpRequest
//
XMLHttpRequest.filesystem = {};
function XMLHttpRequest() {
}

XMLHttpRequest.prototype.open = function(method, url, async) {
    this.status = 200;
    this.readyState = 4;
    this.responseText = XMLHttpRequest.filesystem[url];
};

XMLHttpRequest.prototype.send = function() {
    this.onreadystatechange();
};

// Load yoink.js
var YOINK = eval(fs.readFileSync('yoink.js', 'utf8'));

function loadFile(url, contents) {
    XMLHttpRequest.filesystem[url] = contents;
    var retval;
    YOINK.require([url], function(x) {retval = x});
    assert.ok(retval !== undefined);
    return retval;
}

// Basic tests of YOINK.require()
assert.equal(loadFile('a', '42'), '42');
assert.equal(loadFile('a.json', '42'), 42);
assert.equal(loadFile('a.js', 'define(42);'), 42);

console.log('passed!');

