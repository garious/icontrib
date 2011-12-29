var a = yoink('test/assert.js');
var assert = a.assert;

var good = yoink('test/goodModule.js');
assert(good === 'it worked');

assert(1   == '1');
assert(1  !== '1');
assert(''  == false);
assert('' !== false);

yoink('test/nested/nested.js');


//
// CommonJS and RequireJS assume a 'define' function.
//
var commonjs = function(text, url) {
    return yoink.javascript('var define = function(x) {return x;}; return ' + text, url);
}
var obj = yoink('test/requirejsModule.js', commonjs);
assert(obj.hello === 'world');


// Alternative implementation, a little more complex but works for both yoink and requireJS modules.
var commonjs2 = function(text, url) {
    var s = 'var _r1; var define = function(x) {_r1 = x;}; var _r2 = (function(){' + text + '})(); return _r2 === undefined ? _r1 : _r2';
    return yoink.javascript(s, url);
}
yoink.loaded = [];
yoink.interpreters.js = commonjs2;

var good = yoink('test/goodModule.js');
assert(good === 'it worked');

var obj = yoink('test/requirejsModule.js');
assert(obj.hello === 'world');

// cleanup
yoink.loaded = [];
yoink.interpreters.js = yoink.js;

print('passed!');

