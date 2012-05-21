// Definition of the ToDom interface

// Returns a DOM node for this object
// function ToDom.toDom (me)

var toDomId = Yoink.baseUrl + '/ToDom.js';


// ToDom instance for strings

String.interfaces = String.interfaces || {};

String.interfaces[toDomId] = {
    toDom: function (me) {
        return document.createTextNode(me);
    }
};

// ToDom instance for object
Object.interfaces = Object.interfaces || {};

function addFunctions(div, funcs) {
    var hdr = document.createElement('h3');
    hdr.appendChild(document.createTextNode('Functions'));
    div.appendChild(hdr);

    for (var i = 0; i < funcs.length; i += 1) {
        var x = funcs[i];
        var p = document.createElement('p');
        p.appendChild( document.createTextNode(x) );
        div.appendChild(p);
    }
}

function addConstants(div, constants) {
    var hdr = document.createElement('h3');
    hdr.appendChild(document.createTextNode('Constants'));
    div.appendChild(hdr);

    for (var i = 0; i < constants.length; i += 1) {
        var k = constants[i];
        var p = document.createElement('p');
        p.appendChild( document.createTextNode(k) );
        div.appendChild(p);
    }
}

Object.interfaces[toDomId] = {
    toDom: function (me) {
        var funcs = [];
        var constants = [];
        for (var k in me) {
            if (me.hasOwnProperty(k)) {
                var ty = typeof me[k];
                if (ty === 'function') {
                    funcs.push(k);
                } else {
                    constants.push(k);
                }
            }
        }

        var div = document.createElement('div');
        div.style.marginLeft = '10px';

        var hdr = document.createElement('h2');
        hdr.appendChild(document.createTextNode('JavaScript Module'));
        div.appendChild(hdr);

        if (funcs.length > 0) {
            funcs.sort();
            addFunctions(div, funcs);
        }

        if (constants.length > 0) {
            constants.sort();
            addConstants(div, constants);
        }

        return div;
    }
};

Yoink.define({
    toDomId: toDomId
});

