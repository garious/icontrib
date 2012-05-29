// Definition of the ToDom interface

// Returns a DOM node for this object
// function ToDom.toDom (me)

var toDomId = Yoink.fileUrl;


// ToDom instance for strings

String.interfaces = String.interfaces || {};

String.interfaces[toDomId] = {
    toDom: function (me) {
        return document.createTextNode(me);
    }
};

Yoink.define({
    toDomId: toDomId
});

