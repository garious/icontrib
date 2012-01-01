var a = yoink('assert.js');

var isSet = false;

var f1 = function() {
    isSet = true;
};

// A timer that is already expired is not run synchronously, but scheduled to execute immediately.
setTimeout(f1, 0);
a.assert(isSet === false);


// Queue the test
var f2 = function() {
    a.assert(isSet === true);
    print("passed!");
};

print("wait for it...")
setTimeout(f2, 0);

