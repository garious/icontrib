//
// Layout with hugging and spooning
//

// All combinators are of type "Maybe attrs -> Array a -> a"
//
// hug(  ['a','b','c'])       === 'abc'
// spoon(['a','b','c'])       === 'a\nb\nc'

var deps = [
    'Tag.js',
    'Interface.js',
    'TwoDimensional.js',
    'ToDom.js'
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
            // ys = filter (!= pillow) xs
            var xs = me.subelements;
            var ys = [];
            for (var i = 0; i < xs.length; i += 1) {
                var x = xs[i];
                var iface = I.getInterface(x, Dom.toDomId);
                x = iface && iface.toDom(x) || x;
                if (x.constructor !== pillow) {  // Since DOM elements to not implement ToDom, we unfortunately have to pull pillows explicitly.
                    ys.push(x);
                }
            }
            return Tag.tag('div', {style: {height: me.height + 'px', width: me.width + 'px', position: 'absolute'}}, ys);
    
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
    pillow.interfaces[Dim.twoDimensionalId] = Pillow_TwoDimensional;
    
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
    party.interfaces[Dom.toDomId] = Party_ToDom;
    party.interfaces[Dim.twoDimensionalId] = Party_TwoDimensional;
    
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
        var iface = I.getInterface(x, Dim.twoDimensionalId);
if (!iface) {console.log(x);}
        iface.setPosition(x, {'top': 0, left: dim.width});
    
        var d = iface.getDimensions(x);
        return {
            width: dim.width + d.width,
            height: d.height > dim.height ? d.height : dim.height
        };
    }

    function setHPosBottom(x, dim) {
        var iface = I.getInterface(x, Dim.twoDimensionalId);
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
        var iface = I.getInterface(x, Dim.twoDimensionalId);
        iface.setPosition(x, {'top': dim.height, left: 0});

        var d = iface.getDimensions(x);
        return {
            height: dim.height + d.height,
            width: d.width > dim.width ? d.width : dim.width
        };
    }

    function setVPosRight(x, dim) {
        var iface = I.getInterface(x, Dim.twoDimensionalId);
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
        var iface = I.getInterface(b, Dom.toDomId);
        return iface.toDom(b);
    }
    
    // Concatenate elements vertically and wrap in a DOM element
    function spoon(as, xs) {
        var b = vcat(as, xs);
        var iface = I.getInterface(b, Dom.toDomId);
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

