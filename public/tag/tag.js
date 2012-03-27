// Create a DOM element from a name, attributes object, and array of children.

var deps = [
    'interface.js',
    '2d.js',
    'todom.js'
];

function onReady(I, Dim, Dom) {

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
                    var iface = I.getInterface(x, Dom.ToDomId);
                    e.appendChild(iface ? iface.toDom(x) : x);
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

        // Override constructor so that we can support interfaces
        e.constructor = tag;
    
        return e;
    }

    tag.interfaces = {};

    tag.interfaces[Dim.TwoDimensionalId] = {
    
        // Calculate outer width of a DOM element
        getDimensions: function (me) {
            var sty = me.style;
    
            var width  = parseInt(sty.width,  10) || 0;
            var height = parseInt(sty.height, 10) || 0;
    
            width  += parseInt(sty.marginLeft,   10) || 0;
            width  += parseInt(sty.marginRight,  10) || 0;
            height += parseInt(sty.marginTop,    10) || 0;
            height += parseInt(sty.marginBottom, 10) || 0;
    
            width  += parseInt(sty.paddingLeft,   10) || 0;
            width  += parseInt(sty.paddingRight,  10) || 0;
            height += parseInt(sty.paddingTop,    10) || 0;
            height += parseInt(sty.paddingBottom, 10) || 0;
    
            width  += parseInt(sty.borderLeftWidth,   10) || 0;
            width  += parseInt(sty.borderRightWidth,  10) || 0;
            height += parseInt(sty.borderTopWidth,    10) || 0;
            height += parseInt(sty.borderBottomWidth, 10) || 0;
    
            return {
                width:  width,
                height: height
            };
        },
    
        setPosition: function (me, pos) {
            me.style.position = 'absolute';
    
            if (pos.top !== undefined) {
                me.style.top = pos.top + 'px';
            }
    
            if (pos.left !== undefined) {
                me.style.left = pos.left + 'px';
            }
    
            if (pos.bottom !== undefined) {
                me.style.bottom = pos.bottom + 'px';
            }
    
            if (pos.right !== undefined) {
                me.style.right = pos.right + 'px';
            }
    
            return me;
        }
        
    };
    
    tag.interfaces[Dom.ToDomId] = {
        toDom: function (me) {
            return me;
        }
    };
    
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

}

require(deps, onReady);


