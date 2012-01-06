//
// This file will always expose the latest jQuery as a Yoink module
//

return YOINK.module( ['jquery-1.7.1.min.js'], function(){
    var jq = jQuery;

    // undefine globals
    window.jQuery = undefined;
    window.$ = undefined;

    return jq;
});

