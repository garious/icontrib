// Create a DOM element from a name, attributes object, and array of children.

function text(s) {
    return document.createTextNode(s);
}

function tag(nm, as, xs, es) {
    if (typeof as === 'string' || as && as.constructor === Array) {
        es = xs;
        xs = as;
        as = null;
    }

    // Add attributes
    var e = document.createElement(nm); 
    var k;
    if (as) {
        for (k in as) {
            if (as.hasOwnProperty(k)) {
                if (k === 'style') {
                    var style = as[k];
                    for (var s in style) {
                        e.style[s] = style[s];
                    }
                } else {
                    e.setAttribute(k, as[k]);
                }
            }
        }
    }

    // Add children
    if (xs) {
        if (typeof xs === 'string') {
            e.appendChild(text(xs));
        } else {
            for (var i = 0; i < xs.length; i++) {
                var x = xs[i];
                if (typeof x === 'string') {
                   x = text(x);
                }
                e.appendChild(x);
            }
        }
    }

    // Add event handlers
    if (typeof es === 'object') {
        for (k in es) {
            if (es.hasOwnProperty(k)) {
                e.addEventListener(k, es[k]);
            }
        }
    }

    return e;
}

function mkTag(nm) {
    return function(as, xs, es) {
        return tag(nm, as, xs, es);
    };
}

var Tag = {
    tag:        tag,
    mkTag:      mkTag,
    text:       text
};

var tags = [
    'br', 'hr', 'p', 'div', 'link', 'a', 'img', 'span',
    'form', 'fieldset', 'input', 'label', 'button',
    'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
    'base', 'ul', 'ol', 'li', 'legend', 
    'table', 'th', 'tr', 'td', 'thead', 'tbody', 'tfoot',
    'canvas'
];

for (var i = 0; i < tags.length; i++) {
    var nm = tags[i];
    Tag[nm] = mkTag(nm);
}

define(Tag);


