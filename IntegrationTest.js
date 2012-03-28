// This unit test ties together the frontend and backend and ensures the pages can be rendered
// without errors.
//
// Caution: at the time of this writing, the client has a dependency the google API, which means
//          that executing this test requires an Internet connection.
//
// How the test works:
//
//   The test starts up the web server, sends HTTP GET requests, and renders the returned pages.
//

var Http       = require('http');
var Subprocess = require('child_process');
var Zombie     = require('zombie');
var Assert     = require('assert');

// Server configuration
var serverDir = 'Darwin_Debug/ship';
var dbDir = 'private/db';


function onResponse(res) {
    Assert.strictEqual(res.statusCode, 200);
    res.setEncoding('utf8');

    // Finally ready to start our client tests!
    onServerReady();
}

var triesLeft = 10;
function onResponseError(e) {
    if (e.code === 'ECONNREFUSED' && triesLeft > 0) {
        console.log('Server is not ready yet.  Trying again in 10ms.');
        triesLeft -= 1;
        setTimeout(pingServer, 10);
    } else {
        throw 'Unexpected error: ' + e.message;
    }
}

// Start the web server.  This test will not exit until the server process is killed.
var httpPort = 8890;
var httpHost = 'http://localhost:' + httpPort + '/';
var server = Subprocess.spawn(serverDir + '/icontrib', ['--dbdir=' + dbDir, '--port=' + httpPort]);

// Ping the server until it fails to fail
function pingServer() {
    var serverOptions = {
      host: 'localhost',
      port: httpPort,
      path: '/donor/greg.json'
    };
    var connection = Http.get(serverOptions, onResponse);
    connection.on('error', onResponseError);
}
pingServer();

// Pages to test
var pages = [
    {path: '/'},
    {path: '/Registration'},
    {path: '/Charity?id=usoa'},
    {path: '/Me'},
];

function onServerReady() {

    // Start the client
    var options = {
        silent: true,
        site: httpHost
    };

    var browser = new Zombie()
    
    var pageIndex = 0;
    
    function onLoaded() {
        Assert.strictEqual(browser.statusCode, 200);

        // Oddly, Zombie says we have errors that cannot be reproduced in any real browser.  Filter them:
        var bogusMsg = 'undefined is not a function';
        var errors = browser.errors.filter(function(e) {
            return e.message !== bogusMsg;
        });

        // Verify the page loaded without errors.
        Assert.deepEqual(errors, []);
    
        // Success!  On to the next page.
        pageIndex += 1;
    
        if (pageIndex < pages.length) {
            loadPage(pages[pageIndex]);
        } else {
            server.kill();
            console.log('passed!');
        }
    }
    
    function loadPage(page) {
        console.log('Testing page: ' + page.path);
    
        var callback;
        if (page.onLoaded) {
            callback = function() {
                page.onLoaded();
                onLoaded();
            };
        } else {
            callback = onLoaded;
        }
    
        browser.visit(page.path, options, onLoaded);
    }

    // Oddly, Zombie doesn't respect the 'silent' option until the 2nd page is load.
    // So we seed the system with a page that doesn't print anything.
    pageIndex -= 1;
    browser.visit('/donor/greg.json', options, onLoaded);
}

