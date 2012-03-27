// Definition of the ToDom interface

// Returns a DOM node for this object
// function ToDom.toDom (me)

var ToDomId = baseUrl + '/ToDom.js';


// ToDom instance for strings

String.interfaces = String.interfaces || {};

String.interfaces[ToDomId] = {
    toDom: function (me) {
        return document.createTextNode(me);
    }
};

define({
    ToDomId: ToDomId
});

