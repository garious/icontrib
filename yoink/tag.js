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
//     createElement(x) === tag(x).toDom()
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


var deps = [
    'interface.js',
    'observable.js'
];

var toDomInterface = {
    toDom: function() {}
};

function onReady(iface, observable) {

    // Add all items of style object 'style' to the DOM element 'e'.
    function addStyle(e, style) {
        for (var s in style) {
            if (style.hasOwnProperty(s) && style[s] !== undefined) {
                if (iface.supportsInterface(style[s], observable.observableId)) {
                    e.style[s] = style[s].get();
                    style[s].subscribe(function(obs) {e.style[s] = obs.get();});
                } else {
                    e.style[s] = style[s];
                }
            }
        }
    }

    // Add attribute 'k' with value 'v' to the DOM element 'e'.
    function addAttribute(e, k, v) {
        if (v !== undefined) {
            if (iface.supportsInterface(v, observable.observableId)) {
                e.setAttribute(k, v.get());
                v.subscribe(function(obs) {e[k] = obs.get();});
            } else {
                e.setAttribute(k, v);
            }
        }
    }

    function mkSetChildren(e) {
        return function (obs) {
            e.innerHTML = '';
            var xs = obs.get();
            for (var i = 0; i < xs.length; i++) {
                var x = xs[i];
                e.appendChild(iface.supportsInterface(x, toDomInterface) ? x.toDom() : x);
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
                if (iface.supportsInterface(xs, observable.observableId)) {
                    var xsObs = xs;
                    xs = xsObs.get();
                    xsObs.subscribe(mkSetChildren(e));
                }

                for (var i = 0; i < xs.length; i++) {
                    var x = xs[i];
                    e.appendChild(iface.supportsInterface(x, toDomInterface) ? x.toDom() : x);
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

    //
    // tag({name, attributes, style, contents, handlers})
    //
    function tag(as) {

        if (typeof as === 'string') {
            as = {name: as};
        }

        var me = {
            constructor: tag,
            name:        as.name
        };

        me.toDom = function() {
           return createElement(me);
        };

        me.setPosition = function (pos) {
            if (!me.attributes) {
                me.attributes = {};
            }

            if (!me.style) {
                me.style = {};
            }

            mixin(me.style, pos);

            return me;
        };

        if (as.attributes !== undefined) { me.attributes = as.attributes; }
        if (as.style      !== undefined) { me.style      = as.style; }
        if (as.contents   !== undefined) { me.contents   = as.contents; }
        if (as.handlers   !== undefined) { me.handlers   = as.handlers; }

        return me;
    }

    yoink.define({
        createElement: createElement,
        mixin:         mixin,
        cascadeStyles: cascadeStyles,
        tag:           tag
    });
    
}

yoink.require(deps, onReady);


