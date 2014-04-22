Yoink, a client-side reactive programming library
=======================

observable.js is a general library for reactive programming in JavaScript.  tag.js
applies the reactive programming model to HTML.  It allows the user to write dynamic
content without having to manager event handlers directly.  When an observable value
changes, the HTML that observes it is updated automatically.


Contributing
============

Gladly accepting Pull Requests.

To run the tests on OSX, first install a few prereqs via Homebrew. http://brew.sh

```bash
    # Install Homebrew
    $ ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

    # Install node
    $ brew install node

    # Install JavaScript Lint
    $ brew install homebrew/binary/jsl

    # Run the tests
    $ go test
```
