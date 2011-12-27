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
// yoink, a simple module loader.  XMLHttpRequest is the only dependency.
//

function yoink(url) {

    // If not already loaded
    if (!yoink.loaded[url]) {
        
        // Fetch the module
        var req = new XMLHttpRequest;
        req.open('GET', url, false);
        req.send();

        // Find the directory the file is in.
        var base = url.substring(0, url.lastIndexOf('/'));

        // Provide loaded module with a version of yoink() that pulls modules 
        // relative to the directory of the url we are currently loading.
        var relYoink = function (url) { 
            if (url[0] != '/' && url.indexOf('://') == -1) {
               url = base + '/' + url;
            }
            return yoink(url);
        };

        // Load the module and cache it.
        // Note: Chrome/v8 requires the outer parentheses.  Firefox/spidermonkey does fine without.
        var f = eval('(function (yoink) {' + req.responseText + '})');
        yoink.loaded[url] = f(relYoink);
    }
    
    return yoink.loaded[url];
}

yoink.loaded = {};

