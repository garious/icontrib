//
// Copyright (c) 2011-2012 Greg Fitzgerald, IContrib.org
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this 
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, 
// merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
// persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or 
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
// DEALINGS IN THE SOFTWARE.
//
//
// Module name:
//
//     Tag
//
// Description:
//
//     Tag is a JavaScript module for creating HTML elements.
//     The module exposes two object constructors, 'createElement' and 'tag'.
//     The functions accept the same arguments, an HTML tag name, an attributes
//     object, an array of subelements, and an eventHandlers object.  The
//     difference is that 'tag' postpones the creation of an underlying DOM
//     element, whereas 'createElement' creates and returns the DOM element.
//
//     createElement(x) === toDomElement( tag(x) )
//         where
//             function toDomElement(x) {
//                 var methods = Iface.getInterface(x, Dom.toDomId);
//                 return methods.toDom(x);
//             }
//
//     By postponing the creation of the DOM, we can unit test modules
//     that return tag objects without requiring a browser or a browser
//     simulator such as JsDom or Zombie.  A bare-bones JavaScript interpreter
//     such as Node.js will suffice.
//
//     Q: What if my application dynamically updates other element attributes?
//
//     A: Instead of setting the attribute directly, express the dependency with
//        an Observable variable.  Your event handler should set the observable
//        variable and your tag should be constructed using the observable.  The
//        Tag library will detect the observable attribute and update the DOM
//        element any time its value changes.
//
//
//     Q: Why doesn't tag() automatically create observables for every tag 
//        attribute.
//
//     A: If your application is mostly static content, creating the extra
//        objects could delay startup time and consume memory the application
//        doesn't need.
//
//
//     Q: Why don't tag objects have a toDomElement() method?
//
//     A: Interface implementations come and go.  Avoiding methods allows us to
//        introduce and deprecate interfaces over time without breaking
//        compatibility.  For example, say you want to port your Tag-based JavaScript
//        application to another platform.  Just attach a "ToMyPlatform"
//        interface implementation to tag.constructor.interfaces and you're 
//        done.  No need to version an object prototype or burden others
//        with changes they don't necessarily need.
//
//
//     Q: Interface programing is a pain.  This is stupid.
//
//     A: Indeed, JavaScript doesn't have good support for interface programming.
//        See the Lua programming language's 'metamethods' for an example
//        of how this should work and then please contact your nearest 
//        JavaScript representative to file a complaint.


var deps = [
    'Interface.js',
    'ToDom.js',
    'Observable.js'
];

function mkSetAttribute(e, k, getter) {
    return function (obs) {
        e[k] = getter(obs);
    };
}

function mkSetStyle(e, k, getter) {
    return function (obs) {
        e.style[k] = getter(obs);
    };
}

function onReady(Iface, Dom, Observable) {

    // Add all items of style object 'style' to the DOM element 'e'.
    function addStyle(e, style) {
        for (var s in style) {
            if (style.hasOwnProperty(s) && style[s] !== undefined) {
                var methods = Iface.getInterface(style[s], Observable.observableId);
                if (methods) {
                    e.style[s] = methods.get(style[s]);
                    methods.subscribe(style[s], mkSetStyle(e, s, methods.get));
                } else {
                    e.style[s] = style[s];
                }
            }
        }
    }

    // Add attribute 'k' with value 'v' to the DOM element 'e'.
    function addAttribute(e, k, v) {
        if (v !== undefined) {
            var methods = Iface.getInterface(v, Observable.observableId);
            if (methods) {
                e.setAttribute(k, methods.get(v));
                methods.subscribe(v, mkSetAttribute(e, k, methods.get));
            } else {
                e.setAttribute(k, v);
            }
        }
    }

    function mkSetChildren(e, getter) {
        return function (obs) {
            e.innerHTML = '';
            var xs = getter(obs);
            for (var i = 0; i < xs.length; i++) {
                var x = xs[i];
                var iface = Iface.getInterface(x, Dom.toDomId);
                e.appendChild(iface ? iface.toDom(x) : x);
            }
        };
    }


    // Create a DOM element with tag name 'nm', attributes object 'as', style object 'sty', 
    // an array of subelements 'xs', and an object of event handlers 'es'.
    function createElement(ps) {

        if (typeof ps === 'string') {
            ps = {name: ps};
        }

        // Create DOM node
        var e = document.createElement(ps.name);

        // Add attributes
        var as = ps.attributes;
        var k;
        if (as) {
            for (k in as) {
                if (as.hasOwnProperty(k) && k !== 'style') {
                    addAttribute(e, k, as[k]);
                }
            }
        }

        // Add Style
        if (ps.style) {
            addStyle(e, ps.style);
        }
    
        // Add child elements
        var xs = ps.contents;
        if (xs) {
            if (typeof xs === 'string') {
                e.appendChild(document.createTextNode(xs));
            } else {
                var methods = Iface.getInterface(xs, Observable.observableId);
                if (methods) {
                    var xsObs = xs;
                    xs = methods.get(xsObs);
                    methods.subscribe(xsObs, mkSetChildren(e, methods.get));
                }

                for (var i = 0; i < xs.length; i++) {
                    var x = xs[i];
                    var iface = Iface.getInterface(x, Dom.toDomId);
                    e.appendChild(iface ? iface.toDom(x) : x);
                }
            }
        }
    
        // Add event handlers
        var es = ps.handlers;
        if (typeof es === 'object') {
            for (k in es) {
                if (es.hasOwnProperty(k)) {
                    e.addEventListener(k, es[k]);
                }
            }
        }

        return e;
    }

    // Overwrite 'obj' with defined keys in 'newObj'
    function mixin(obj, newObj) {
        for (var k in newObj) {
            if (newObj.hasOwnProperty(k) && newObj[k] !== undefined) {
                obj[k] = newObj[k];
            }
        }

        return obj;
    }

    // left-fold style objects
    // cascadeStyles(xs) === {} `mixin` xs[0] `mixin` xs[1] `mixin` ... `mixin` xs[-1]
    function cascadeStyles(xs) {
        return xs.reduce(mixin, {});
    }

    function tag(as) {

        if (typeof as === 'string') {
            as = {name: as};
        }

        var me = {
            constructor: tag,
            name:        as.name
        };

        if (as.attributes !== undefined) { me.attributes = as.attributes; }
        if (as.style      !== undefined) { me.style      = as.style; }
        if (as.contents   !== undefined) { me.contents   = as.contents; }

        return me;
    }

    tag.interfaces = {};

    tag.interfaces[Dom.toDomId] = {
        toDom: createElement
    };
    

    //
    // tag(nm, attributes, subelements, eventHandlers)
    //

    // Create an object with tag name 'nm', attributes object 'as', an array of 
    // subelements 'xs', and an object of event handlers 'es'.
    function tag_deprecated(nm, as, xs, es) {
        if (typeof as === 'string' || as && as.constructor === Array) {
            es = xs;
            xs = as;
            as = undefined;
        }

        return tag({
            name: nm,
            attributes: as,
            style: as && as.style,
            contents: xs,
            handlers: es
        });
    }

    Yoink.define({
        createElement: createElement,
        mixin:         mixin,
        cascadeStyles: cascadeStyles,
        tag:           tag_deprecated,
        tag1:          tag
    });
    
}

Yoink.require(deps, onReady);


