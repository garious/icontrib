// Create a DOM element from a name, attributes object, and array of children.

var text = function(s) {
    return document.createTextNode(s);
};

var tag = function(nm, as, xs) {
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
        xs.forEach(function(x) {
            if (typeof x === 'string') {
               x = text(x);
            }
            e.appendChild(x);
        });
    }
    return e
};

var mkTag = function(nm) {
    return function(as, xs) {
        return tag(nm, as, xs)
    };
};

var stylize = function(style, e) {
    for (var k in style) {
        e.style[k] = style[k];
    }
    return e;
};

var TAG = {
    tag:        tag,
    mkTag:      mkTag,
    text:       text,
    stylize:    stylize,
};

var tags = ['br', 'p', 'div', 'link', 'a', 'img', 'form', 'input', 'h1', 'h2', 'h3', 'h4', 'base', 'ul', 'ol', 'li'];

tags.forEach(function(nm) {
    TAG[nm] = mkTag(nm);
});

return TAG;


