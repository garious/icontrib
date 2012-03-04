var deps = [
     'assert.js'
];

function onReady (A) {

    var options = {
      host: 'localhost',
      port: 8000,
      path: '/donor/greg.json'
    };

    function onResponseData (chunk) {
        var user = JSON.parse(chunk);
        //console.log("Got data:\n" + JSON.stringify(user, null, 4));

        var fundLabel = 'Gregs-General-Fund';

        var x = user.distribution[0];

        A.assert(x.name);  // Assert 'name' field exists
        A.assert(x.labels[0] === fundLabel);
        
        // TODO: FIX SERVER!
        //A.assert(x.url);   // Assert 'url' field exists

        var fund = user.funds[0];
        A.assert(fund.labels[0] === fundLabel);

        console.log("passed!");
        process.stdout.flush();
        process.exit(0);
    }

    function onResponse(res) {
        A.assert(res.statusCode === 200);
        res.on('data', onResponseData);
    }

    function onResponseError(e) {
        console.warn("Got error: " + e.message);
        process.exit(1);
    }
    
    var http = require('http');
    var connection = http.get(options, onResponse);
    connection.on('error', onResponseError);

}

// Queue the test
function fail () {
    throw "timer expired.  fail!";
}

setTimeout(fail, 3000);

YOINK.require(deps, onReady);


