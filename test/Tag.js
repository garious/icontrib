// Create a DOM element from a name, attributes object, and array of children.
function tag(nm, as, xs) {
    if (as && as.constructor === Array) {
        xs = as;
        as = null;
    }

    var e = document.createElement(nm); 
    if (as) {
        for (var k in as) {
            e.setAttribute(k, as[k]);
        }
    }
    if (xs) {
        xs.forEach(function(x) {e.appendChild(x);});
    }
    return e
};

var mkTag = function(nm) {
    return function(as, xs) {
        return tag(nm, as, xs)
    };
};

Yoink.define({
    tag:    tag,
    mkTag:  mkTag,
    br:     mkTag('br'),
    p:      mkTag('p'),
    div:    mkTag('div'),
    text:   function(s) {return document.createTextNode(s); },
});

