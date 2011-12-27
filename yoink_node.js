// A script to make the node.js command-line interface more like v8 and spidermonkey

var fs = require('fs');


// spidermonkey has a 'print' function.
global.print = console.log;

// yoink.js requires XMLHttpRequest
if (typeof XMLHttpRequest == 'undefined') {
    global.XMLHttpRequest = function() {
       this.open = function(method, url, b) { this.responseText = fs.readFileSync(url, 'utf8'); };
       this.send = function(){};
    };
}

// Preload yoink so that we can add the function as a global variable
var cnts = fs.readFileSync('yoink.js', 'utf8');
eval(cnts);
global.yoink = yoink;

// Assume any arguments are scripts that should be executed
var args = process.argv.splice(2);

args.forEach(function(path) {
    var cnts = fs.readFileSync(path, 'utf8');
    eval(cnts);
});


