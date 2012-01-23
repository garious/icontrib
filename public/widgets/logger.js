var deps = [
    '/js/console.js'
]; 

function onReady() {
    define({
        debug: function( msg) { console.log(msg); },
        warn: function( msg) { console.warn(msg); },
        error: function( msg) { console.error(msg); },
        info: function( msg) { console.info(msg); }
    });
}

require(deps, onReady);

