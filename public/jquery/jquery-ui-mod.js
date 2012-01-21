// TODO: Generalize this function
function exportJQuery(text, yoink, callback) {
    YOINK.interpreters.js(text + '\ndefine( jQuery.noConflict(true) );', yoink, callback);
}

function onReady($) {
    // jQuery UI requires jQuery.  Therefore its 'onReady' callback must return a second set 
    // of dependencies and callback.

    // jQuery UI requires that jQuery be in the global namespace when it is interpreted.
    window.jQuery = $;

    var deps = [
        {path: 'jquery-ui-1.8.16.custom.min.js', interpreter: exportJQuery}
    ];

    define(deps, function(jQuery) { return jQuery.noConflict(true); });
}

// Create a separate resource loader, so that jQuery UI can whomp its personal copy of jQuery.
var loader = YOINK.resourceLoader();
loader.getResources(['/jquery/jquery-mod.js'], onReady);

// Do not return anything.  This tells the module loader that we will call 'define()'
// once our unique loader has loaded jQuery and jQuery UI.

