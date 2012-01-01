//
// Copyright (c) 2011 Greg Fitzgerald, IContrib.org
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
        js: function(text, loader) {
            // Load the module
            // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
            var f = eval('(function (yoink, loader) {' + text + '})');
            return f(mkYoink(loader), loader);
        },
    }
    
    function ResourceLoader(base, cache, interpreters) {
        this.base = base;
        this.cache = cache || {};
        this.interpreters = interpreters || defaultInterpreters;
        return this;
    };

    ResourceLoader.prototype.interpret = function(rsc, url, f) {
        // Look up an interpreter for the URL's file extension
        if (!f) {
            var ext = url.substring(url.lastIndexOf('.') + 1, url.length).toLowerCase();
            f = this.interpreters[ext];
        }

        // Interpret the resource
        if (f) {
            if (f.length == 1) {
                rsc = f(rsc);
            } else {
                // Provide loaded module with a version of loader that pulls modules 
                // relative to the directory of the url we are currently loading.
                var base = url.substring(0, url.lastIndexOf('/'));
                var subloader = new ResourceLoader(base, this.cache, this.interpreters);
                rsc = f(rsc, subloader);
            }
        }

        return rsc;
    };

    ResourceLoader.prototype.resolve = function(url) {
        if (this.base !== undefined && url[0] !== '/' && url.indexOf('://') === -1) {
           url = this.base + '/' + url;
        }
        return url;
    };

    ResourceLoader.prototype.sync = function(url, f) {
        url = this.resolve(url);
        var rsc = this.cache[url];

        // If not already cached
        if (rsc === undefined) {
    
            // Fetch the resource
            var req = new XMLHttpRequest();
            req.open('GET', url, false);
            req.send();

            rsc = this.interpret(req.responseText, url, f);
    
            // Cache the result
            this.cache[url] = rsc;
        }
        
        return rsc;
    };

    ResourceLoader.prototype.async = function(url, f, callback) {
        url = this.resolve(url);
        var rsc = this.cache[url];

        // If not already cached
        if (rsc === undefined) {
    
            // Fetch the resource
            var req = new XMLHttpRequest();
            var loader = this;
            req.onreadystatechange = function() {
                if (req.readyState === 4) {
                    rsc = loader.interpret(req.responseText, url, f);

                    // Cache the result
                    loader.cache[url] = rsc;

                    callback(rsc);
                }
            };
            req.open('GET', url, true);
            req.send();
        } else {
            // Push the callback to the event queue.
            setTimeout(function(){callback(rsc);}, 0);
        };
    };

    // Constructor without exposing 'new' keyword
    function createResourceLoader(base, cache, interpreters) {
       return new ResourceLoader(base, cache, interpreters);
    };

    // For backward compatibility
    function mkYoink(loader) {
        var yoink          = function(url, f) { return loader.sync(url, f); };
        yoink.loaded       = loader.cache;
        yoink.interpreters = loader.interpreters;
        yoink.json         = defaultInterpreters.json;
        yoink.javascript   = defaultInterpreters.js;
        return yoink;
    };

    return {
       createResourceLoader: createResourceLoader,
       interpreters: defaultInterpreters,
       yoink: mkYoink(createResourceLoader()),  // For backward compatibility
    };
})();

// For backward compatibility
var yoink = YOINK.yoink;

