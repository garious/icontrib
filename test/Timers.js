var deps = [
    'Assert.js'
];

function onReady(Assert) {
    var isSet = false;

    Assert.assert(isSet === false);

    function f1() {
        isSet = true;
    };
    
    // Queue the test
    function f2() {
        Assert.assert(isSet === true);
        print("passed!");
    };

    // A timer that is already expired is not run synchronously, but scheduled to execute immediately.
    setTimeout(f1, 0);
    
    print("wait for it...")
    setTimeout(f2, 0);
}

YOINK.require(deps, onReady);

