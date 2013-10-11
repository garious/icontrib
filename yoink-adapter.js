// A test harness that lets us execute Yoink modules

(function () {
    "use strict";

    var path = require('path');
    var optparse = require('./opt-parse');
    
    var switches = [
        ['--modspec STRING', 'Declares JSON module spec']
    ];
    
    // Create a new OptionParser.
    var parser = new optparse.OptionParser(switches);
    
    var modspec = {};
    parser.on('modspec', function(name, value) {
        modspec = JSON.parse(value);
    });

    var files = parser.parse(process.argv);

    var fs = require('fs');

    // yoink.js requires XMLHttpRequest
    if (typeof XMLHttpRequest === 'undefined') {
        global.XMLHttpRequest = function () {
            this.open = function (method, url, async) {
                if (async) {
                    var me = this; // Ensure timer callback executes in the context of this 'this'
                    setTimeout(function () {
                        // If root directory, look for file in --modspec
                        if (url.charAt(0) === '/') {

                            // Look for a matching key, and if so, replace it with its value in the URL
                            for (var p in modspec) {
                                var u = url.replace(p, modspec[p]);
                                if (path.existsSync(u)) {
                                    url = u;
                                    break;
                                }
                            }
                        }
                        me.responseText = fs.readFileSync(url, 'utf8');
                        if (me.onreadystatechange) {
                            me.readyState = 4;
                            me.onreadystatechange();
                        }
                    }, 0);
                } else {
                    this.responseText = fs.readFileSync(url, 'utf8');
                }
            };
            this.send = function () {};
        };
    }

    // Preload yoink so that we can add the function as a global variable
    var cnts = fs.readFileSync(__dirname + '/yoink.js', 'utf8');
    var YOINK = eval(cnts);

    // Assume any arguments are modules that should be executed
    var args = files.splice(2);

    YOINK.require(args, function () {

        // Print the result of each module.
        for (var k in arguments) {
            var v = arguments[k];
            if (typeof v === 'string') {
                console.log(v);
            } else {
                console.log('passed!');
            }
        }
    });

})();

