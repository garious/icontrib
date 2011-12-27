var a = yoink('test/assert.js');
var assert = a.assert;

var good = yoink('test/goodModule.js');
assert(good === 'it worked');

assert(1   == '1');
assert(1  !== '1');
assert(''  == false);
assert('' !== false);

yoink('test/nested/nested.js');

print('passed!');

