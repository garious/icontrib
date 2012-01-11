//
// This file will always expose the latest jQuery as a module
//

return {
    deps: ['jquery-1.7.1.min.js', 'jquery-ui-1.8.16.custom.min.js'],

    callback: function() {
        return jQuery.noConflict(true);
    }
};

