The Yoink Web Framework
=======================

HTML is for web documents.  JavaScript is for web applications.  The Yoink framework
lets us write all client-side code exclusively in JavaScript.  The framework also
provides a set of libraries for Reactive Programming in JavaScript.  Reactive
programs scale well and can be tested outside the browser.  Reactive elements do
not have explicit event handlers.  Instead, they use the Publish-Subscribe pattern
to observe other reactive elements.


JavaScript Module Loader
----

yoink.js is a standalone module loader.  See loader/README.md for more details.


Client-side reactive programming library
----

The 'yoink' directory contains client-side JavaScript libraries.  observable.js
is a general library for reactive programming in JavaScript.  tag.js applies the
reactive programming model to HTML.  It allows the user to write dynamic content
without having to manager event handlers directly.  When an observable value
changes, the HTML that observes it is updated automatically.


Sever-side Go AppEngine library
----

JsAppServer is a Go package for building Go AppEngine web servers using Yoink.
When the user requests a URL, JsAppServer looks for a resource with the same 
name and a '.js' extension.  If it finds one, the server wraps the JavaScript
with an HTML page that uses yoink.js to load and run that JavaScript file.  The
JavaScript file then calls 'yoink.define()' with a DOM element to display in the
HTML body.


Unit-testing library for client-side JavaScript
-----

Instead of passing 'yoink.define()' a DOM element, you also have the option to
pass it an object that implements a 'toDom()' method, which returns the DOM
element to display.  This is usually preferred because it allows you to test
the guts of your application outside the browser.  The Yoink Framework comes
with an adapter for Node.js called 'yoink-adapter.js' and a simple assertion
module called 'assert.js'.  With it, you can easily write server-side tests
for your JavaScript.  For example:

```bash
   $ cat mylib.js
```

```javascript
    yoink.define({
       fortyTwo: function() {return 42;}
    });
```

```bash
    $ cat mylib_test.js
```

```javascript
    yoink.require(['assert.js', 'mylib.js'], function(assert, mylib) {
        assert.assertEq(mylib.fortyTwo(), 42);
        yoink.define('passed!');
    });
```

```bash
    $ node yoink-adapater.js mylib_test.js
    passed!
```    

Contributing
============

Gladly accepting Pull Requests.

To run the yoink tests on OSX, first install a few prereqs via Homebrew. http://brew.sh

```bash
    # Install Homebrew
    $ ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

    # Install node
    $ brew install node

    # Install JavaScript Lint
    $ brew install homebrew/binary/jsl

    # Run the tests
    $ cd yoink
    $ go test
```
