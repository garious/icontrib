
Yoink.js
========

yoink.js is a small and simple module loader for JavaScript.

Yoink modules are simple.  Here is the "Hello World" of Yoink modules:

  ~~~javascript
  define( document.createTextNode("Hello world!") );
  ~~~

Add Yoink, load your module, and use it to construct the DOM.

  ~~~html
  <html>
      <body>
          <script type="text/javascript" src="yoink-min.js"></script>
          <script type="text/javascript">
              YOINK.require(['helloworld.js'], function(HELLO) {
                  document.body.appendChild(HELLO);
              });
          </script>
      </body>
  </html>
  ~~~

Yoink modules may load other modules.  Modules are downloaded and interpreted in
parallel.


  ~~~javascript
  var loader = YOINK.resourceLoader();
  loader.getResources(['helloworld.js', 'goodbye.js'], function(HELLO, GOODBYE) {
      document.body.appendChild(HELLO);
      document.body.appendChild(GOODBYE);
  });
  ~~~

To download a module that tells you what modules to download:

  ~~~javascript
  require(['a.js'], function(A) {

      require(A.moreDeps, function(B, C, D) {
          define( document.createTextNode(A.message + B.message) );
      });

  });
  ~~~

Modules know where they are.  Modules are loaded with a local variable 'baseUrl'
that tells module authors where the module is with respect to the root directory.  Module
authors can use this value to reference external resources, such as an image file inside
the module directory.

  ~~~javascript
  var e = document.createElement('img');
  e.src = baseUrl + '/favicon.png'; 
  define( e );
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

