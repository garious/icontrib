// A test harness that lets us execute Yoink modules

(function () {

    var fs = require('fs');
    
    // yoink.js requires XMLHttpRequest
    if (typeof XMLHttpRequest === 'undefined') {
        global.XMLHttpRequest = function() {
           this.open = function(method, url, async) {
               if (async) {
                   var me = this; // Ensure timer callback executes in the context of this 'this'
                   setTimeout(function() {
                       // Replace leading slash with path to the 'root' directory.
                       url = url.replace(/^\//, '../');

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
           this.send = function(){};
        };
    }
    
    // Preload yoink so that we can add the function as a global variable
    var cnts = fs.readFileSync(__dirname + '/../Yoink/Yoink.js', 'utf8');
    eval(cnts);
    
    // Assume any arguments are modules that should be executed
    var args = process.argv.splice(2);
    
    YOINK.require(args, function() {
    
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

