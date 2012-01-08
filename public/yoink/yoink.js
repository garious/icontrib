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

    var defaultInterpreters = {
        json: function(text) {
            return JSON.parse(text);
        },
        js: function(text, yoink, callback) {
            // Load the module
            // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
            var f = eval('(function (baseUrl) {' + text + '})');
            var mod = f(yoink.base);
            if (mod && mod.deps && mod.callback) {
                yoink(mod.deps, function() {
                    callback(mod.callback.apply(null, arguments));
                });
            } else {
                callback(mod);
            }
        },
    };

    var clone = function(o1) {
        var o2 = {};
        for (k in o1) {
            o2[k] = o1[k];
        }
        return o2;
    };
    
    var ResourceLoader = function(base, cache, interpreters) {
        this.base = base || '';
        this.cache = cache || {};
        this.interpreters = interpreters || clone(defaultInterpreters);
        return this;
    };

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
                var yoink = function(urls, f) {return getResources.call(subloader, urls, f);};
                yoink.base = base;
                interpreter(rsc, yoink, callback);
            }
        },

        resolve: function(url) {
            var p = url.path || url;
            var f = url.interpreter || null;
            if (this.base !== '' && p[0] !== '/' && p.indexOf('://') === -1) {
               p = this.base + '/' + p;
            }
            return {path: p, interpreter: f};
        },

        // Download a resource synchronously
        getResourceSync: function(url) {
            url = this.resolve(url);
            var rsc = this.cache[url.path];

            // If not already cached
            if (rsc === undefined) {
    
                // Fetch the resource
                var req = new XMLHttpRequest();
                req.open('GET', url.path, false);
                req.send();

                var getResources = function(urls, callback) {
                   var rscs = this.getResourcesSync(urls);
                   callback.apply(null, rscs);
                };
                this.interpret(req.responseText, url.path, url.interpreter, getResources, function(r){rsc = r;});
    
                // Cache the result
                this.cache[url.path] = rsc;
            }
            
            return rsc;
        },

        getResourcesSync: function(urls) {
            var rscs = [];
            var len = urls.length;
            for (var i = 0; i < len; i++) {
                rscs[i] = this.getResourceSync(urls[i]);
            }
            return rscs;
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

        // Download a resource asynchronously
        getResource: function(url, callback) {
            url = this.resolve(url);
            var rsc = this.cache[url.path];

            // If not already cached
            if (rsc === undefined) {
                var loader = this;
                this.getFile(url.path, function(text) {
                    var getResources = function(urls, f) {
                       this.getResources(urls, f);  // Important: use 'this', not 'loader' so that we can overwrite 'this' later
                    };
                    loader.interpret(text, url.path, url.interpreter, getResources, function(rsc) {
                        // Cache the result
                        loader.cache[url.path] = rsc;
                        callback(rsc);
                    });
                });
            } else {
                // Push the callback to the event queue.
                setTimeout(function(){callback(rsc);}, 0);
            }
        },

        // Download resources in parallel asynchronously
        getResources: function(urls, callback) {
            var rscs = [];
            var cnt = 0;
            var len = urls.length;
            var loader = this;
            var getResources = function(us, f) {
               this.getResources(us, f);  // Important: use 'this', not 'loader' so that we can overwrite 'this' later
            };
            var interpretRsc = function(i) {
                var u = urls[i];
                loader.interpret(rscs[i], u.path, u.interpreter, getResources, function(rsc) {
                     // Cache the result
                     loader.cache[u.path] = rsc;
                     rscs[i] = rsc;
                     i++;
                     if (i === len) {
                         callback.apply(null, rscs);
                     } else {
                         interpretRsc(i);
                     };
                });
            };
            var mkHandler = function(i) {
                return function(rsc) {
                     rscs[i] = rsc;
                     cnt++;
                     
                     // After all files have been downloaded, interpret each in order.
                     if (cnt === len) {
                         if (len > 0) {
                           interpretRsc(0);
                         }
                     }
                };
            };
            for (var i = 0; i < len; i++) {
                urls[i] = this.resolve(urls[i]);
                this.getFile(urls[i].path, mkHandler(i));
            }
        },
    };

    // Constructor without exposing 'new' keyword
    function resourceLoader(base, cache, interpreters) {
       return new ResourceLoader(base, cache, interpreters);
    };

    // convenience function for creating a yoink module
    function module(deps, f) {
       return {deps: deps, callback: f};
    };

    return {
       resourceLoader: resourceLoader,
       module: module,
       interpreters: defaultInterpreters,
    };
})();

