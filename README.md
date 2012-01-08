
Yoink.js
========

yoink.js is a small and simple module loader for JavaScript.

* Yoink modules are simple.  Here is the "Hello World" of Yoink modules:

  ~~~javascript
  return document.createTextNode("Hello world!");
  ~~~

* HTML is minimal.  Add Yoink, load a module, and use it to construct the DOM.

  ~~~html
  <html>
      <body>
          <script type="text/javascript" src="https://raw.github.com/garious/yoink/master/yoink-min.js"></script>
          <script type="text/javascript">
              var loader = YOINK.resourceLoader();
              loader.getResource('helloworld.js', function(hello) {
                  document.body.appendChild(hello);
              });
          </script>
      </body>
  </html>
  ~~~


* Easy to unit-test modules.  Use *Sync methods and enjoy no dependencies on 'document' or 'window'.

  ~~~javascript
              var loader = YOINK.resourceLoader();
              loader.getResourceSync('helloworld.js', function(hello) {
                  document.body.appendChild(hello);
              });
  ~~~


* Resources loaded in parallel.

  ~~~javascript
              var loader = YOINK.resourceLoader();
              loader.getResources(['helloworld.js', 'goodbye.js'], function(hello, goodbye) {
                  document.body.appendChild(hello);
                  document.body.appendChild(goodbye);
              });
  ~~~

* Intuitive semantics.  
  
  * Resources are downloaded in parallel.
  * Resources are interpreted in order.
  * Cached resources are never reinterpreted.

* Yoink modules are scalable.  When you return the 'module' object, Yoink will first
  download its dependencies.

  ~~~javascript
  var deps = ['fileNextToHello.js'];

  function onReady(neighbor) {
      return document.createTextNode(neighbor.message);
  }

  return {deps: deps, callback: onReady};
  ~~~

* Modules know where they are.  Modules are loaded with a local variable 'baseUrl'
  that tells module authors where the module is with respect to the root directory.  Module
  authors can use this value to reference external resources, such as an image file inside
  the module directory.

  ~~~javascript
  var e = document.createElement('img');
  e.src = baseUrl + '/favicon.png'; 
  return e;
  ~~~


Contributing
============

To develop on OSX Lion, download and build dependencies with 'homebrew'.

http://mxcl.github.com/homebrew/

    $ /usr/bin/ruby -e "$(curl -fsSL https://raw.github.com/gist/323731)"


Testing
-------

yoink.js is unit-tested with a few dependencies as possible.  We test on
Chrome and Firefox via Node.js and SpiderMonkey, respectively.

    $ brew install node
    $ brew install spidermonkey

    $ make test


Distributuion
-------------

Get YUI Compressor to make yoink-min.js

    $ brew install yuicompressor

    $ make compress

