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
    'TwoDimensional.js'
];

function onReady(Tag, I, Dim) {

    // pillow(w, h)
    //
    //     Create empty space of 'w' pixels wide and 'h' pixels tall.
    function pillow(w, h) {
        if (h === undefined) {
            h = w;
        }
        // Chrome won't render an object that is 0 pixels high or wide
        h = h || 1;
        w = w || 1;
        return Tag.tag('div', {style: {width: w + 'px', height: h + 'px'}});
    }
    
    // Concatenate elements
    function cat(as, xs, setPos) {
        for (var i = 0; i < xs.length; i += 1) {
            setPos(xs[i]);
        }
        return Tag.tag('div', xs);
    }
    
    // Set the horizontal position of a 2D element
    function setHPos(x) {
        var iface = I.getInterface(x, Dim.twoDimensionalId);
        iface.setPosition(x, {'float': 'left', clear: 'none'});
    }

    // Concatenate elements horizontally
    function hug(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        return cat(as, xs, setHPos);
    }
    
    // Set the vertical position of a 2D element
    function setVPos(x) {
        var iface = I.getInterface(x, Dim.twoDimensionalId);
        iface.setPosition(x, {'float': 'left', clear: 'both'});
    }

    function setVPosRight(x) {
        var iface = I.getInterface(x, Dim.twoDimensionalId);
        iface.setPosition(x, {'float': 'right', clear: 'both'});
    }
    
    // Concatenate elements vertically
    function spoon(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        var setPos = as.align === 'right' ? setVPosRight : setVPos;
        return cat(as, xs, setPos);
    }
    
    define({
        hug:    hug,
        spoon:  spoon,
        pillow: pillow
    });
}

require(deps, onReady);

