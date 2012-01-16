var deps = [
    '/js/console.js'
]; 

function onReady() {
    return {
        debug: function( msg) { console.log(msg); },
        warn: function( msg) { console.warn(msg); },
        error: function( msg) { console.error(msg); },
        info: function( msg) { console.info(msg); }
    };
}

define(deps, onReady);

