//
// A JavaScript library for 2D layout
//

// hcat(['a','b','c']) === 'abc'
// vcat(['a','b','c']) === 'a\nb\nc'
//
// To put space between elements, use the gap(nPixels) function.

var deps = [
    'tag.js',
    'interface.js',
    'observable.js'
];

function onReady(tag, iface, observable) {

    // gap(n)
    //
    //     Create empty space of 'n' pixels wide and 'n' pixels tall.
    function gap(n) {
        return tag.tag({name: 'div', style: {width: n + 'px', height: n + 'px'}});
    }
    
    // Concatenate elements
    function cat(as, xs, setPos) {
        var ys = xs;
        if (iface.supportsInterface(ys, observable.IObservable)) {
            xs = ys.get();
        }
        for (var i = 0; i < xs.length; i += 1) {
            setPos(xs[i]);
        }
        return tag.tag({name: 'div', contents: ys});
    }
    
    // Set the horizontal position of a 2D element
    function setHPos(x) {
        x.setPosition({
            cssFloat: 'left',
            clear: 'none'
        });
    }

    // Concatenate elements horizontally
    function hcat(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        return cat(as, xs, setHPos);
    }
    
    // Set the vertical position of a 2D element
    function setVPos(x) {
        x.setPosition({
            cssFloat: 'left',
            clear: 'both'
        });
    }

    function setVPosRight(x) {
        x.setPosition({
            cssFloat: 'right',
            clear: 'both'
        });
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
    
    yoink.define({
        hcat: hcat,
        vcat: vcat,
        gap: gap
    });
}

yoink.require(deps, onReady);

