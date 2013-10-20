var deps = [
    'assert.js'
];

function onReady(assert) {
    var isSet = false;

    assert.assertEq(isSet, false);

    function f1() {
        isSet = true;
    }
    
    // Queue the test
    function f2() {
        assert.assertEq(isSet, true);
        yoink.define("passed!");
    }

    // A timer that is already expired is not run synchronously, but scheduled to execute immediately.
    setTimeout(f1, 0);
    
    console.log("wait for it...");
    setTimeout(f2, 0);
}

YOINK.require(deps, onReady);

