//
// This file will always expose the latest jQuery as a module
//

return {
    deps: ['jquery-1.7.1.min.js'],

    callback: function() {
        var jq = jQuery;

        // undefine globals
        window.jQuery = undefined;
        window.$ = undefined;

        return jq;
    },
};

