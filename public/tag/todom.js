// Definition of the ToDom interface

var ToDom = {
    // Returns a DOM node for this object
    toDom:    function (me) {},

    // If this widget is displayed as a web page, what string do you want in the title bar?
    // Return 'undefined' to have the browser display the default title.
    getTitle: function (me) {}
};


// ToDom instance for strings
var String_ToDom = {
    toDom: function (me) {
        return document.createTextNode(me);
    },
    getTitle: function (me) {
        return undefined;
    }
};

String.interfaces = String.interfaces || [
    {'interface': ToDom, 'instance': String_ToDom}
];


define({
    ToDom: ToDom
});

