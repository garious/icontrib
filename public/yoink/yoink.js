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
// yoink, a simple resource loader.  XMLHttpRequest, console.log() and setTimeout() are the only dependencies.
//

//Debugging methods - log information to console (if available)
if (!window.console) {
    console = {};
}

console.log = console.log || function () { };
console.warn = console.warn || function () { };
console.error = console.error || function () { };
console.info = console.info || function() { };

var YOINK = (function() {

    var yoinkMod = function(mod, yoink, callback) {
        if (mod && mod.deps && mod.callback) {
            yoink(mod.deps, function() {
                var m = mod.callback.apply(null, arguments);
                yoinkMod(m, yoink, callback);
            });
        } else {
            callback(mod);
        }
    };

    var defaultInterpreters = {
        json: function(text) {
            return JSON.parse(text);
        },
        js: function(text, yoink, callback) {
            // Load the module
            // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
			try {
				var f = eval('(function (baseUrl) {' + text + '})');
					
				var mod = f(yoink.base);
                                yoinkMod(mod, yoink, callback);
                        } 
			catch(err) {
				console.error("Error loading the following code");
				console.error(text);
				console.error(err);
				throw err;
			}
        }
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

            // Normalize the path
            p = p.replace(/[^/]+[/]\.\.[/]/g,'');  // Remove redundant '%s/..' items.
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

        // Download resources in parallel asynchronously
        getResources: function(urls, callback) {
            var rscs = [];         // For the results of interpreting files
            var cnt = 0;           // For counting what we've downloaded
            var len = urls.length; // How many things we need to interpret
            var getResources = function(us, f) {
                this.getResources(us, f);  // Important: use 'this', not 'loader' so that we can overwrite 'this' later
            };
            var loader = this;
            var onInterpreted = function(i, files) {
                 i++;  // Index of the next item to interpret

                 // Skip cached resources
                 while (i < len && files[i] === null) {
                     i++;
                 }

                 if (i === len) {
                     callback.apply(null, rscs);
                 } else {
                     interpretFile(i, files);
                 }
            }
            var mkOnInterpreted = function(p, i, files) {
                 return function(rsc) {
                    // If resource does not return a result, force it to 'null' so that we have something to cache.
                    if (rsc === undefined) {
                       rsc = null;
                    }
                    rscs[i] = rsc;
                    loader.cache[p] = rsc; // Cache the result
                    onInterpreted(i, files);
                 }
            };
            var interpretFile = function(i, files) {
                var u = urls[i];
	        console.log("yoink: interpreting '" + urls[i].path + "'");
                loader.interpret(files[i], u.path, u.interpreter, getResources, mkOnInterpreted(u.path, i, files));
            };
            var onDownloaded = function(files) {
                 cnt++;
                 
                 // After all files have been downloaded, interpret each in order.
                 if (cnt === len) {
                     onInterpreted(-1, files);
                 }
            };
            var mkOnDownloaded = function(i, files) {
                return function(str) {
                     files[i] = str;
                     onDownloaded(files);
                };
            };
            var download = function(i, files) {
                urls[i] = loader.resolve(urls[i]);
                var p = urls[i].path;
                var rsc = loader.cache[p];
                if (rsc === undefined) {
                    loader.getFile(p, mkOnDownloaded(i, files));
                } else {
                    files[i] = null;
                    rscs[i] = rsc;
                    onDownloaded(files);  // Skip downloading
                }
            };
            var files = [];        // For downloaded files
            for (var i = 0; i < len; i++) {
                download(i, files);
            }
        },
    };

    // Constructor without exposing 'new' keyword
    function resourceLoader(base, cache, interpreters) {
       return new ResourceLoader(base, cache, interpreters);
    };

    return {
       resourceLoader: resourceLoader,
       interpreters: defaultInterpreters,
    };
})();

