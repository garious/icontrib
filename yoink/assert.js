// Throw an exception and error message if two arguments are not equal
function assertEq(actual, expected) {
   if (actual === expected) {
       return;
   } else if (typeof actual === 'object') {
       var k;
       for (k in expected) {
           if (expected.hasOwnProperty(k) && k !== 'constructor') {
                assertEq(actual[k], expected[k]);
           }
       }
       for (k in actual) {
           if (actual.hasOwnProperty(k) && k !== 'constructor') {
                if (expected[k] === undefined && actual[k] !== undefined) {
                    var stack = new Error().stack;
                    throw("Assertion failed.\nObject is missing key: " + k + '\n\n' + (new Error()).stack);
                }
           }
       }
   } else {
       throw("Assertion failed.\nExpected:\n" + expected + "\nBut got:\n" + actual + '\n\n' + (new Error()).stack);
   }
}

yoink.define({
    assertEq: assertEq
});

