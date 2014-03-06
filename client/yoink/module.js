//
// Experiment: a module constructor for modules.  This object implements the ToDom interface
//

function module(o1) {
    var o2 = {
        constructor: module,
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

    for (var k in o1) {
        if (o1.hasOwnProperty(k)) {
            o2[k] = o1[k];
        }
    }

    return o2;
}

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

yoink.define({
    module: module
});

