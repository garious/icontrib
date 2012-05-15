// This unit test aims to be the JavaScript equivalent to SiteTest.hs in Haskell
//
// The test starts up the web server, sends HTTP GET requests, and verifies the contents
// of the response.

var Http       = require('http');
var Subprocess = require('child_process');
var Assert     = require('assert');

// Server configuration
var serverPath = process.argv[2];
var dbDir = 'private/db';


//
// Test
//
function onResponseData (chunk) {
    var user = JSON.parse(chunk);

    // If you need to debug, uncomment the 'stringify' line to pretty-print the response packet.
    // Note: we assume the full response fits in one chunk.  At the time of this writing, I
    //       believe the buffer size was something very large, like 200 KB.

    //console.log("Got data:\n" + JSON.stringify(user, null, 4));


    var x = user.distribution[0];

    Assert.ok(x.name);  // Assert 'name' field exists

    // Verify the fund is tagged with one of the user's labels.
    var fundLabel = 'Gregs-General-Fund';
    Assert.strictEqual(x.labels[0],         fundLabel);
    Assert.strictEqual(user.funds[0].label, fundLabel);

    // That's it, all tests passed!
    console.log("passed!");
    process.exit(0);
}

function onResponse(res) {
    Assert.strictEqual(res.statusCode, 200);
    res.on('data', onResponseData);
}

var triesLeft = 10;
function onResponseError(e) {
    if (e.code === 'ECONNREFUSED' && triesLeft > 0) {
        console.log('Server is not ready yet.  Trying again in 10ms.');
        triesLeft -= 1;
        setTimeout(onReady, 10);
    } else {
        throw 'Unexpected error: ' + e.message;
    }
}

// Start the web server
var httpPort = 8889;
var server = Subprocess.spawn(serverPath, ['--dbdir=' + dbDir, '--port=' + httpPort]);
server.on('exit', function (code, signal) {
    console.log('child process terminated due to receipt of signal ' + signal);
});
server.stdout.on('data', function (data) {
    console.log('stdout: ' + data);
});

// The server outputs its log messages on stderr.  Uncomment this line to see them.
//server.stderr.on('data', function (data) {
//    console.log('stderr: ' + data);
//});

// Connect to the server
function onReady() {

    var options = {
      host: 'localhost',
      port: httpPort,
      path: '/donor/greg.json'
    };

    var connection = Http.get(options, onResponse);
    connection.on('error', onResponseError);
}
setTimeout(onReady, 10);  // Give the server a few milliseconds to start.

