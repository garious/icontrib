Building the web server
-----------------------

The IContrib.org web server is a standalone command-line executable written in the Haskell
programming language.  To compile and run the server, you will first need to install
 Haskell Platform:

http://hackage.haskell.org/platform/


Then clone the source code repository:

    git clone git@github.com:garious/icontrib.git


If on Ubuntu, install a few more prereqs:

    sudo apt-get install libssl-dev libcrypto++-dev


Using GNU Make 3.81, run the rule that downloads and builds the project dependencies.

    make deps


To build the web server and run the test suites:

    make tree_all


To run the web server:

    make serve

