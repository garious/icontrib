var a = yoink('assert.js', 'test');
var assert = a.assert;

var good = yoink('goodModule.js', 'test');
assert(good === 'it worked');

assert(1   == '1');
assert(1  !== '1');
assert(''  == false);
assert('' !== false);

yoink('nested/nested.js', 'test');

print('passed!');

