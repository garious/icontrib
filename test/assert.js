var assert = function(b) {
    if (!b) {
        throw "assertion failed";
    }
};

return {
   assert: assert,
};

