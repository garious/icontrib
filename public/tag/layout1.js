//
// Layout with hugging and spooning
//

// All combinators are of type "Maybe attrs -> Array a -> a"
//
// hug(  ['a','b','c'])       === 'abc'
// spoon(['a','b','c'])       === 'a\nb\nc'

var deps = [
    'tag.js',
    'interface.js',
    '2d.js',
    'todom.js'
];

function onReady(Tag, I, Dim, Dom) {

    // a TwoDimensional instance for the Pillow class
    var Pillow_TwoDimensional = {
        getDimensions: function (me) {
            return {
                width: me.width,
                height: me.height
            };
        },

        setPosition: function (me, pos) {
            return me;
        }
    };
    
    // a TwoDimensional instance for the Party class
    var Party_TwoDimensional = Pillow_TwoDimensional;
    
    // a ToDom instance for the Party class
    var Party_ToDom = {
        toDom: function (me) {
            var div = Tag.tag('div');
            div.style.height = me.height + 'px';
            div.style.width = me.width + 'px';
            div.style.position = 'absolute';
    
            // ys = filter (!= pillow) xs
            var xs = me.subelements;
            for (var i = 0; i < xs.length; i += 1) {
                var x = xs[i];
                var iface = I.getInterface(x, Dom.ToDomId);
                x = iface && iface.toDom(x) || x;
                if (x.constructor !== pillow) {  // Since DOM elements to not implement ToDom, we unfortunately have to pull pillows explicitly.
                    div.appendChild(x);
                }
            }
            return div;
        },
        getTitle: function (me) {
            return undefined;
        }
    };
    
    // pillow(w, h)
    //
    //     Create empty space of 'w' pixels wide and 'h' pixels tall.  Pillow elements 
    //     are not added to the DOM, and are only used for managing space.
    function pillow(w, h) {
        if (h === undefined) {
            h = w;
        }
        return {
            constructor: pillow,
            width: w,
            height: h 
        };
    }
    
    pillow.interfaces = {};
    pillow.interfaces[Dim.TwoDimensionalId] = Pillow_TwoDimensional;
    
    // party(attrs, subelements)
    //
    //    a placeholder for visual elements to snuggle
    function party(as, xs) {
    
        return {
            constructor: party,
            width: as.width,
            height: as.height,
            subelements: xs
        };
    }
    
    party.interfaces = {};
    party.interfaces[Dom.ToDomId] = Party_ToDom;
    party.interfaces[Dim.TwoDimensionalId] = Party_TwoDimensional;
    
    // Concatenate elements
    function cat(as, xs, setPos) {
    
        // dim = reduce(setPos, xs, (0,0))
        var dim = {width: 0, height: 0};
        for (var i = 0; i < xs.length; i += 1) {
            var x = xs[i];
            dim = setPos(x, dim);
        }
    
        return party(dim, xs);
    }
    
    // Set the horizontal position of a 2D element
    function setHPos(x, dim) {
        var iface = I.getInterface(x, Dim.TwoDimensionalId);
        iface.setPosition(x, {'top': 0, left: dim.width});
    
        var d = iface.getDimensions(x);
        return {
            width: dim.width + d.width,
            height: d.height > dim.height ? d.height : dim.height
        };
    }

    function setHPosBottom(x, dim) {
        var iface = I.getInterface(x, Dim.TwoDimensionalId);
        iface.setPosition(x, {'bottom': 0, left: dim.width});
    
        var d = iface.getDimensions(x);
        return {
            width: dim.width + d.width,
            height: d.height > dim.height ? d.height : dim.height
        };
    }
    
    // Concatenate elements horizontally
    function hcat(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        var setPos = as.align === 'bottom' ? setHPosBottom : setHPos;
        return cat(as, xs, setPos);
    }
    
    // Set the vertical position of a 2D element
    function setVPos(x, dim) {
        var iface = I.getInterface(x, Dim.TwoDimensionalId);
        iface.setPosition(x, {'top': dim.height, left: 0});

        var d = iface.getDimensions(x);
        return {
            height: dim.height + d.height,
            width: d.width > dim.width ? d.width : dim.width
        };
    }

    function setVPosRight(x, dim) {
        var iface = I.getInterface(x, Dim.TwoDimensionalId);
        iface.setPosition(x, {'top': dim.height, right: 0});

        var d = iface.getDimensions(x);
        return {
            height: dim.height + d.height,
            width: d.width > dim.width ? d.width : dim.width
        };
    }
    
    // Concatenate elements vertically
    function vcat(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        var setPos = as.align === 'right' ? setVPosRight : setVPos;
        return cat(as, xs, setPos);
    }
    
    // Concatenate elements horizontally and wrap in a DOM element
    function hug(as, xs) {
        var b = hcat(as, xs);
        var iface = I.getInterface(b, Dom.ToDomId);
        return iface.toDom(b);
    }
    
    // Concatenate elements vertically and wrap in a DOM element
    function spoon(as, xs) {
        var b = vcat(as, xs);
        var iface = I.getInterface(b, Dom.ToDomId);
        return iface.toDom(b);
    }
    
    define({
        hcat:   hcat,
        vcat:   vcat,
        hug:    hug,
        spoon:  spoon,
        pillow: pillow
    });
}

require(deps, onReady);

