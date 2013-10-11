function assert(b) {
    if (!b) {
        throw "assertion failed";
    }
};

Yoink.define({
   assert: assert,
});

