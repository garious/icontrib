var deps = [
    '/tag/tag.js'
];

function onReady(E) {
    var map = function mapObject (type, input, output, func) {
        for(var prop in type) {
            if(type.hasOwnProperty(prop)) {
                if(type[prop] === null) {
                    if(output[prop]) {
                        output[prop] = func(prop, input[prop], output[prop]);
                    } else {
                        output[prop] = func(prop, input[prop], null);
                    }
                } else if(typeof(type[prop]) == "object") {
                    if(output[prop]) {
                        output[prop] = mapObject(type[prop], input[prop], output[prop], func);
                    } else {
                        output[prop] = mapObject(type[prop], input[prop], {}, func);
                    }
                } 
            }
        }
        return output;
    };
    var toVal = function(name, val, oval) {
        return val.value;
    };
    var toInput = function (name, val, oval) { 
        return E.input({}); 
    };
    define({
        map: map,
        toInput: toInput,
        toVal: toVal
    });
}

require(deps, onReady);

