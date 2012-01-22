//
// Copyright (c) 2011-2012 Greg Fitzgerald, IContrib.org
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this 
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, 
// merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
// persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or 
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
// DEALINGS IN THE SOFTWARE.
//

//
// yoink, a simple resource loader.  XMLHttpRequest is the only dependency.
//


var YOINK = (function() {

    var console = window && window.console || {log: function(){}};

    var defaultInterpreters = {
        json: function(text) {
            return JSON.parse(text);
        },
        js: function(text, require, callback) {
            // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
            var f_str = '(function (baseUrl, define, require) {' + text + '})';
            var f = eval(f_str);
            f(require.base, callback, require);
        }
    };

    // Special handling for Internet Explorer
    if (window && window.execScript) {
        defaultInterpreters.json = function(text) {
            var f_str = '(function () { return ' + text + ';})';
            window.execScript('_iesucks = ' + f_str);
            return _iesucks();
        };

        defaultInterpreters.js = function(text, require, callback) {
            var f_str = '(function (baseUrl, define, require) {' + text + '})';
            window.execScript('_iesucks = ' + f_str);
            var f = _iesucks;
            f(require.base, callback, require);
        };
    }

    function clone(o1) {
        var o2 = {};
        for (k in o1) {
            o2[k] = o1[k];
        }
        return o2;
    }

    function interpret(rsc, url, interpreter, interpreters, cache, callback) {
        // Look up an interpreter for the URL's file extension
        if (!interpreter) {
            var ext = url.substring(url.lastIndexOf('.') + 1, url.length).toLowerCase();
            interpreter = interpreters[ext] || function(x){return x;};
        }

        // Interpret the resource
        if (interpreter.length === 1) {
            callback( interpreter(rsc) );
        } else {
            // Provide loaded module with a version of loader that pulls modules 
            // relative to the directory of the url we are currently loading.
            var base = url.substring(0, url.lastIndexOf('/'));
            var require = mkGetResources(base, cache, interpreters);
            require.base = base;
            interpreter(rsc, require, callback);
        }
    }

    // Download a text file asynchronously
    function getFile(path, callback) {
        var req = new XMLHttpRequest();
        req.onreadystatechange = function() {
            if (req.readyState === 4) {
                callback(req.responseText);
            }
        };
        req.open('GET', path, true);
        req.send();
    }

    // System-wide cache of what to do once a resource has been downloaded.
    var plans = {};

    function interpretFile(interpreters, cache, u, str) {
        console.log("yoink: interpreting '" + u.path + "'");
        interpret(str, u.path, u.interpreter, interpreters, cache, function(rsc) {
            cache[u.path] = rsc; // Cache the result
            // Execute the plan
            var plan = plans[u.path];
            delete plans[u.path];
            plan(rsc);
        }); 
    }

    function getResource(interpreters, cache, url, onInterpreted) {
        var p = url.path;
        var rsc = cache[p];
        if (rsc === undefined) {
            // Is anyone else already downloading this file?
            var plan = plans[p];
            if (plan === undefined) {
                // Create a plan for what we will do with this module
                plans[p] = function(rsc) {
                    onInterpreted(rsc);
                }
                getFile(p, function(str){
                    interpretFile(interpreters, cache, url, str);
                });
            } else {
                // Add ourselves to the plan.  The plan is effectively a FIFO queue of actions.
                plans[p] = function(rsc) {
                    plan(rsc);
                    onInterpreted(rsc);
                };
            }
        } else {
            onInterpreted(rsc);  // Skip downloading
        }
    }

    function resolve(base, url) {
        var p = url.path || url;
        var f = url.interpreter || null;
        if (base !== '' && p.charAt(0) !== '/' && p.indexOf('://') === -1) {
           p = base + '/' + p;
        }

        // Normalize the path
        p = p.replace(/[^\/]+[\/]\.\.[\/]/g,'');  // Remove redundant '%s/..' items.
        return {path: p, interpreter: f};
    }

    function mkGetResources(base, cache, interpreters) {
        return function(urls, callback) {
            var rscs = [];         // For the results of interpreting files
            var cnt = 0;           // For counting what we've downloaded
            var len = urls.length; // How many things we need to interpret

            function mkOnInterpreted(i) {
                 return function(rsc) {
                     rscs[i] = rsc;
                     cnt++;  // Index of the next item to interpret

                     if (cnt === len) {
                         callback.apply(null, rscs);
                     }
                 };
            }

            for (var i = 0; i < len; i++) {
                var u = resolve(base, urls[i]);
                getResource(interpreters, cache, u, mkOnInterpreted(i));
            }
        };
    }
    
    // Resource Loader constructor
    function resourceLoader(base, cache, interpreters) {
        var base = base || '';
        var cache = cache || {};
        var interpreters = interpreters || clone(defaultInterpreters);

        return {getResources: mkGetResources(base, cache, interpreters)};
    }

    return {
       resourceLoader: resourceLoader,
       interpreters: defaultInterpreters
    };
})();

