var deps = [
    '/tag/tag.js'
];

function onReady(E) {
    var map = function mapObject (type, input, output, func) {
        for(var prop in type) {
            if(type.hasOwnProperty(prop)) {
                if(type[prop] === null) {
                    output[prop] = func(prop, input[prop]);
                } else if(typeof(type[prop]) == "object") {
                    output[prop] = mapObject(type[prop], input[prop], {}, func);
                } 
            }
        }
        return output;
    };
    var toVal = function(name, val) {
        return val.value;
    };
    var toInput = function (name, val) { 
        return E.input({}); 
    };
    return {
        map: map,
        toInput: toInput,
        toVal: toVal
    };
}

define(deps, onReady);

