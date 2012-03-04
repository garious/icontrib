function assert(b) {
    if (!b) {
        throw "assertion failed";
    }
}

define({
   assert: assert
});

