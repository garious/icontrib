// Definition of the ToDom interface

// Returns a DOM node for this object
// function ToDom.toDom (me)

var toDomId = {
    toDom: function() {}
};


// ToDom instance for strings
String.prototype.toDom = function () {
    return document.createTextNode(this);
};

yoink.define({
    toDomId: toDomId
});

