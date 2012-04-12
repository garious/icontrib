
var deps = [
    'Interface.js',
    'TwoDimensional.js',
    'ToDom.js',
    'Observable.js'
];

function mkSetAttribute(e, k, getter) {
    return function (obs) {
        e.setAttribute(k, getter(obs));
    };
}

function mkSetStyle(e, k, getter) {
    return function (obs) {
        e.style[k] = getter(obs);
    };
}

function onReady(I, Dim, Dom, Observable) {

    // Add attribute 'k' with value 'v' to the given DOM element 'e'.
    function addAttribute(e, k, v) {
        var methods;
        if (k === 'style') {
            var style = v;
            for (var s in style) {
                if (style.hasOwnProperty(s) && style[s] !== undefined) {
                    methods = I.getInterface(style[s], Observable.observableId);
                    if (methods) {
                        e.style[s] = methods.get(style[s]);
                        methods.subscribe(style[s], mkSetStyle(e, s, methods.get));
                    } else {
                        e.style[s] = style[s];
                    }
                }
            }
        } else if (v !== undefined) {
            methods = I.getInterface(v, Observable.observableId);
            if (methods) {
                e.setAttribute(k, methods.get(v));
                methods.subscribe(v, mkSetAttribute(e, k, methods.get));
            } else {
                e.setAttribute(k, v);
            }
        }
    }

    // Create a DOM element with tag name 'nm', attributes object 'as', an array of 
    // subelements 'xs', and an object of event handlers 'es'.
    function tag(nm, as, xs, es) {

        if (typeof as === 'string' || as && as.constructor === Array) {
            es = xs;
            xs = as;
            as = null;
        }
    
        // Create DOM node
        var e = document.createElement(nm); 

        // Add attributes
        var k;
        if (as) {
            for (k in as) {
                if (as.hasOwnProperty(k)) {
                    addAttribute(e, k, as[k]);
                }
            }
        }
    
        // Add children
        if (xs) {
            if (typeof xs === 'string') {
                e.appendChild(document.createTextNode(xs));
            } else {
                for (var i = 0; i < xs.length; i++) {
                    var x = xs[i];
                    var iface = I.getInterface(x, Dom.toDomId);
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

        return {element: e, constructor: tag};
    }

    tag.interfaces = {};

    tag.interfaces[Dim.twoDimensionalId] = {
    
        setPosition: function (me, pos) {
            var sty = me.element.style;

            if (pos['float'] !== undefined) {
                sty['float'] = pos['float'];
            }

            if (pos.clear !== undefined) {
                sty.clear = pos.clear;
            }
    
            return me;
        }
        
    };
    
    tag.interfaces[Dom.toDomId] = {
        toDom: function (me) {
            return me.element;
        }
    };
    

    // Create an object with tag name 'nm', attributes object 'as', an array of 
    // subelements 'xs', and an object of event handlers 'es'.
    function tag1(nm, as, xs, es) {
        if (typeof as === 'string' || as && as.constructor === Array) {
            es = xs;
            xs = as;
            as = null;
        }

        // Normalize attributes
        // Note: clone(as)?
        as = as || {};
        as.style = as.style || {};

        var sty = as.style;
        var w;

        if (sty.margin !== undefined && String(sty.margin).search('auto') === -1)  {
             w = parseInt(sty.margin, 10) + 'px';
             if (sty.marginLeft === undefined) {
                 sty.marginLeft = w;
             }
             if (sty.marginRight === undefined) {
                 sty.marginRight = w;
             }
             if (sty.marginTop === undefined) {
                 sty.marginTop = w;
             }
             if (sty.marginBottom === undefined) {
                 sty.marginBottom = w;
             }
        }

        if (sty.padding !== undefined)  {
             w = parseInt(sty.padding, 10) + 'px';
             if (sty.paddingLeft === undefined) {
                 sty.paddingLeft = w;
             }
             if (sty.paddingRight === undefined) {
                 sty.paddingRight = w;
             }
             if (sty.paddingTop === undefined) {
                 sty.paddingTop = w;
             }
             if (sty.paddingBottom === undefined) {
                 sty.paddingBottom = w;
             }
        }

        if (sty.border !== undefined)  {
             w = parseInt(sty.border, 10) + 'px';
             if (sty.borderLeftWidth === undefined) {
                 sty.borderLeftWidth = w;
             }
             if (sty.borderRightWidth === undefined) {
                 sty.borderRightWidth = w;
             }
             if (sty.borderTopWidth === undefined) {
                 sty.borderTopWidth = w;
             }
             if (sty.borderBottomWidth === undefined) {
                 sty.borderBottomWidth = w;
             }
        }

        return {
            constructor: tag1,
            name: nm,
            attributes: as,
            subelements: xs,
            handlers: es
        };
    }

    tag1.interfaces = {};

    tag1.interfaces[Dim.twoDimensionalId] = {
        setPosition: function (me, pos) {
            return tag.interfaces[Dim.twoDimensionalId].setPosition({element: me.attributes}, pos);
        }
    };
    
    tag1.interfaces[Dom.toDomId] = {
        toDom: function (me) {
            return tag(me.name, me.attributes, me.subelements, me.handlers).element;
        }
    };
    
    function mkTag(nm) {
        return function(as, xs, es) {
            return tag1(nm, as, xs, es);
        };
    }
    
    var Tag = {
        tag:        tag1,
        mkTag:      mkTag
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


