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

/*jslint vars: true, evil: true, regexp: true, browser: true*/

var YOINK = (function () {
    'use strict';

    var console = (typeof window !== 'undefined' && window.console) || {log: function () {}};

    var defaultInterpreters = {
        json: function (text) {
            return JSON.parse(text);
        },
        js: function (text, require, callback, params) {
            // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
            var f_str = '(function (baseUrl, define, require, params) {"use strict";' + text + '})';
            var f = eval(f_str);
            f(require.base, callback, require, params);
        }
    };

    // Special handling for Internet Explorer
    if (typeof window !== 'undefined' && window.execScript) {
        defaultInterpreters.js = function (text, require, callback, params) {
            var f_str = '(function (baseUrl, define, require, params) {' + text + '})';
            /*global iesucks: true*/
            window.execScript('iesucks = ' + f_str);
            var f = iesucks;
            f(require.base, callback, require, params);
        };
    }

    function clone(o1) {
        var o2 = {};
        var k;
        for (k in o1) {
            if (o1.hasOwnProperty(k)) {
                o2[k] = o1[k];
            }
        }
        return o2;
    }

    // Forward-declare mutual recursion
    var mkGetResources;

    function interpret(rsc, url, params, interpreter, interpreters, cache, moduleCache, callback) {
        // Look up an interpreter for the URL's file extension
        if (!interpreter) {
            var ext = url.substring(url.lastIndexOf('.') + 1, url.length).toLowerCase();
            interpreter = interpreters[ext] || function (x) { return x; };
        }

        // Interpret the resource
        if (interpreter.length === 1) {
            callback(interpreter(rsc));
        } else {
            // Provide loaded module with a version of loader that pulls modules 
            // relative to the directory of the url we are currently loading.
            var base = url.substring(0, url.lastIndexOf('/'));
            var require = mkGetResources(base, cache, moduleCache, interpreters);
            require.base = base;
            interpreter(rsc, require, callback, params);
        }
    }

    // Download a text file asynchronously
    function getFile(path, callback) {
        var req = new XMLHttpRequest();
        req.onreadystatechange = function () {
            if (req.readyState === 4) {
                callback(req.responseText, req.status || 200);
            }
        };
        req.open('GET', path, true);
        req.send();
    }

    // System-wide cache of what to do once a resource has been downloaded.
    var plans = {};

    function interpretFile(interpreters, cache, moduleCache, u, str, httpCode, callback) {
        if (httpCode >= 200 && httpCode < 300) {
            console.log("yoink: interpreting '" + u.path + "'");
            interpret(str, u.path, u.params, u.interpreter, interpreters, cache, moduleCache, callback);
        } else if (u.onError) {
            var rsc = u.onError(httpCode);
            callback(rsc);
        } else {
            throw httpCode;
        }
    }

    function evaluateModule(f, url, params, cache, moduleCache, interpreters, callback) {
        // Provide loaded module with a version of loader that pulls modules 
        // relative to the directory of the url we are currently loading.
        var base = url.substring(0, url.lastIndexOf('/'));
        var require = mkGetResources(base, cache, moduleCache, interpreters);
        f(base, callback, require, params);
    }

    function getResource(interpreters, cache, moduleCache, url, onInterpreted) {
        var p = url.path;

        // A new callback that executes the plan created later in this function.
        function callback(rsc) {
            cache[p] = rsc; // Cache the result
            // Execute the plan
            var plan = plans[p];
            delete plans[p];
            plan(rsc);
        }

        var rsc = cache[p];
        if (rsc === undefined) {
            // Is anyone else already downloading this file?
            var plan = plans[p];
            if (plan === undefined) {
                // Create a plan for what we will do with this module
                plans[p] = function (rsc) {
                    onInterpreted(rsc);
                };

                if (moduleCache[p]) {
                    // Is this in our module cache?
                    console.log("yoink: executing preloaded module '" + p + "'");
                    evaluateModule(moduleCache[p], p, url.params, cache, moduleCache, interpreters, callback);
                } else {
                    getFile(p, function (str, httpCode) {
                        interpretFile(interpreters, cache, moduleCache, url, str, httpCode, callback);
                    });
                }
            } else {
                // Add ourselves to the plan.  The plan is effectively a FIFO queue of actions.
                plans[p] = function (rsc) {
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
        var ps = url.params || {};
        var f = url.interpreter || null;
        if (base !== '' && p.charAt(0) !== '/' && p.indexOf('://') === -1) {
            p = base + '/' + p;
        }

        // Normalize the path
        p = p.replace(/[^\/]+[\/]\.\.[\/]/g, '');  // Remove redundant '%s/..' items.
        return {path: p, params: ps, interpreter: f, onError: url.onError};
    }

    mkGetResources = function (base, cache, moduleCache, interpreters) {

        function getResources(urls, callback) {
            var len = urls.length; // How many things we need to interpret
            var i;
            var rscs = [];         // For the results of interpreting files
            var cnt = 0;           // For counting what we've downloaded

            function mkOnInterpreted(i) {
                return function (rsc) {
                    rscs[i] = rsc;
                    cnt += 1;  // Index of the next item to interpret

                    if (cnt === len) {
                        callback.apply(null, rscs);
                    }
                };
            }

            if (len === 0) {
                callback();
            } else {
                for (i = 0; i < len; i += 1) {
                    var u = resolve(base, urls[i]);
                    getResource(interpreters, cache, moduleCache, u, mkOnInterpreted(i));
                }
            }
        }

        return getResources;
    };

    // Resource Loader constructor
    function resourceLoader(base, cache, moduleCache, interpreters) {
        base = base || '';
        cache = cache || {};
        moduleCache = moduleCache || {};
        interpreters = interpreters || clone(defaultInterpreters);

        return {getResources: mkGetResources(base, cache, moduleCache, interpreters)};
    }

    function require(urls, callback) {
        return resourceLoader().getResources(urls, callback);
    }

    return {
        require: require,
        resourceLoader: resourceLoader,
        interpreters: defaultInterpreters
    };
}());

