The Yoink Web Framework
=======================

HTML is for web documents.  JavaScript is for web applications.  The Yoink framework
lets us code exclusively in JavaScript.  The framework also provides a set of
libraries for Reactive Programming in JavaScript.  Reactive programs scale well
and can be tested outside the browser.  Reactive elements do not have explicit 
event handlers.  Instead, they use the Publish-Subscribe pattern to observe other
reactive elements.

examples
----

Each subdirectory of the 'examples' directory contains a standalone webserver
in 'server.go' and client code in its 'index.js'.  'hello' is a minimal webpage
using Yoink.  'canvas' introduces the fundamental 'stdlib' library 'dom.js'.
Finally, the 'layout' example shows off 'layout.js' and how a more typical Yoink
webpage is composed.


jsappserver
----

JsAppServer is a Go package for building web servers that use Yoink.  When the
user requests a URL, JsAppServer looks for a resource with the same name and a
'.js' extension.  If it finds one, the server wraps the JavaScript with an HTML
page that uses yoink.js to load and run that JavaScript file.  The JavaScript
file then calls `define()` with a DOM element to display in the HTML body.


jsok
----

The 'jsok' directory contains a Go library that allows you to easily unit-test
Yoink modules.

Instead of passing `define()` a DOM element, you also have the option to
pass it an object that implements a 'render()' method, which returns the DOM
element to display.  This is usually preferred because it allows you to test
the guts of your application outside the browser.  The Yoink Framework comes
with an adapter for Node.js called 'yoink-adapter.js' and a simple assertion
module called 'assert.js'.  With it, you can easily write server-side tests
for your JavaScript.  For example:

```bash
   $ cat mylib.js
```

```javascript
    define({
       fortyTwo: function() {return 42;}
    });
```

```bash
    $ cat mylib_test.js
```

```javascript
    require(['assert.js', 'mylib.js'], function(assert, mylib) {
        assert.eq(mylib.fortyTwo(), 42);
        define('passed!');
    });
```

```bash
    $ node yoink-adapater.js mylib_test.js
    passed!
```    


loader
----

The 'loader' directory contains a minimal asynchronous module loader.


stdlib
----

The 'stdlib' directory contains client-side JavaScript libraries.  observable.js is
a general library for reactive programming in JavaScript.  dom.js applies the
reactive programming model to HTML.


Running the stdlib tests
------------

To develop on OSX, install dependencies with Homebrew. http://brew.sh

```bash
    # Install Homebrew
    $ ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

    # Install node
    $ brew install node

    # Install JavaScript Lint
    $ brew install homebrew/binary/jsl

    # Run the tests
    $ cd stdlib
    $ go test
```
