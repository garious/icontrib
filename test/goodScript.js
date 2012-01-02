//
// Synchronous loading
//
var loader = YOINK.resourceLoader();
var assert = loader.getResourceSync('assert.js').assert;
var good = loader.getResourceSync('goodModule.js');
assert(good === 'it worked');

assert(1   == '1');
assert(1  !== '1');
assert(''  == false);
assert('' !== false);

loader.getResourceSync('nested/nested.js');


//
// RequireJS assumes a 'define' function.
//
var rjs = function(text, yoink, callback) {
    YOINK.interpreters.js('var define = function(x) {return x;}; return ' + text, yoink, callback);
}
var obj = loader.getResourceSync({path: 'requirejsModule.js', interpreter: rjs});
assert(obj.hello === 'world');


//
// Alternative implementation, a little more complex but works for both yoink and requireJS modules.
//
var rjsLoader = YOINK.resourceLoader();
var rjs2 = function(text, yoink, callback) {
    var s = 'var _r1; var define = function(x) {_r1 = x;}; var _r2 = (function(){' + text + '})(); return _r2 === undefined ? _r1 : _r2';
    return YOINK.interpreters.js(s, yoink, callback);
}
rjsLoader.interpreters.js = rjs2;

var good = rjsLoader.getResourceSync('goodModule.js');
assert(good === 'it worked');

var obj = rjsLoader.getResourceSync('requirejsModule.js');
assert(obj.hello === 'world');

print('passed!');

