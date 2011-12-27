
Yoink.js
========

yoink.js is yet another module loader for JavaScript.

* Yoink is small, simple and readable.  Minified version is just 384 bytes!

* Yoink modules are simple.  Here is the "Hello World" of Yoink modules:

    return "Hello world!";

* HTML is minimal.  Add Yoink, load a module, and add its return value to the DOM.

    <html>
        <body>
            <script type="text/javascript" src="https://raw.github.com/garious/yoink/master/yoink-min.js"></script>
            <script type="text/javascript">
                var msg = yoink('helloworld.js');
                document.body.appendChild( document.createTextNode(msg) );
            </script>
        </body>
    </html>

* Yoink modules are scalable.  Yoink loads sub-modules in the context of the
  module directory, not the caller's directory.



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

