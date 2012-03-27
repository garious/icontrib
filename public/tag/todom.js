// Definition of the ToDom interface

var ToDom = {
    // Returns a DOM node for this object
    toDom:    function (me) {},

    // If this widget is displayed as a web page, what string do you want in the title bar?
    // Return 'undefined' to have the browser display the default title.
    getTitle: function (me) {}
};

var ToDomId = baseUrl + '/ToDom.js';


// ToDom instance for strings

String.interfaces = String.interfaces || {};

String.interfaces[ToDomId] = {
    toDom: function (me) {
        return document.createTextNode(me);
    },
    getTitle: function (me) {
        return undefined;
    }
};

define({
    ToDom: ToDom,
    ToDomId: ToDomId
});

