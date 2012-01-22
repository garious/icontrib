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

    // System-wide cache of what to do once a resource has been downloaded.
    var plans = {};
    
    function ResourceLoader(base, cache, interpreters) {
        this.base = base || '';
        this.cache = cache || {};
        this.interpreters = interpreters || clone(defaultInterpreters);
        return this;
    }

    ResourceLoader.prototype = {
        interpret: function(rsc, url, interpreter, getResources, callback) {
            // Look up an interpreter for the URL's file extension
            if (!interpreter) {
                var ext = url.substring(url.lastIndexOf('.') + 1, url.length).toLowerCase();
                interpreter = this.interpreters[ext] || function(x){return x;};
            }

            // Interpret the resource
            if (interpreter.length === 1) {
                callback( interpreter(rsc) );
            } else {
                // Provide loaded module with a version of loader that pulls modules 
                // relative to the directory of the url we are currently loading.
                var base = url.substring(0, url.lastIndexOf('/'));
                var subloader = new ResourceLoader(base, this.cache, this.interpreters);
                var require = function(urls, f) {return getResources.call(subloader, urls, f);};
                require.base = base;
                interpreter(rsc, require, callback);
            }
        },

        resolve: function(url) {
            var p = url.path || url;
            var f = url.interpreter || null;
            if (this.base !== '' && p.charAt(0) !== '/' && p.indexOf('://') === -1) {
               p = this.base + '/' + p;
            }

            // Normalize the path
            p = p.replace(/[^\/]+[\/]\.\.[\/]/g,'');  // Remove redundant '%s/..' items.
            return {path: p, interpreter: f};
        },

        // Download a text file asynchronously
        getFile: function(path, callback) {
            var req = new XMLHttpRequest();
            req.onreadystatechange = function() {
                if (req.readyState === 4) {
                    callback(req.responseText);
                }
            };
            req.open('GET', path, true);
            req.send();
        },

        // Download and interpret resources in parallel
        getResources: function(urls, callback) {
            var rscs = [];         // For the results of interpreting files
            var cnt = 0;           // For counting what we've downloaded
            var len = urls.length; // How many things we need to interpret
            var getResources = function(us, f) {
                this.getResources(us, f);  // Important: use 'this', not 'loader' so that we can overwrite 'this' later
            };
            var loader = this;

            function onInterpreted(i, rsc) {
                 rscs[i] = rsc;
                 cnt++;  // Index of the next item to interpret

                 if (cnt === len) {
                     callback.apply(null, rscs);
                 }
            }

            function interpretFile(u, str) {
                console.log("yoink: interpreting '" + u.path + "'");
                loader.interpret(str, u.path, u.interpreter, getResources, function(rsc) {
                    loader.cache[u.path] = rsc; // Cache the result
                    // Execute the plan
                    var plan = plans[u.path];
                    delete plans[u.path];
                    plan(rsc);
                }); 
            }

            function download(loader, i) {
                urls[i] = loader.resolve(urls[i]);
                var p = urls[i].path;
                var rsc = loader.cache[p];
                if (rsc === undefined) {
                    // Is anyone else already downloading this file?
                    var plan = plans[p];
                    if (plan === undefined) {
                        // Create a plan for what we will do with this module
                        plans[p] = function(rsc) {
                            onInterpreted(i, rsc);
                        }
                        loader.getFile(p, function(str){
                            interpretFile(urls[i], str);
                        });
                    } else {
                        // Add ourselves to the plan.  The plan is effectively a FIFO queue of actions.
                        plans[p] = function(rsc) {
                            plan(rsc);
                            onInterpreted(i, rsc);
                        };
                    }
                } else {
                    onInterpreted(i, rsc);  // Skip downloading
                }
            }

            for (var i = 0; i < len; i++) {
                download(loader, i);
            }
         }
    };

    // Constructor without exposing 'new' keyword
    function resourceLoader(base, cache, interpreters) {
       return new ResourceLoader(base, cache, interpreters);
    }

    return {
       resourceLoader: resourceLoader,
       interpreters: defaultInterpreters
    };
})();

