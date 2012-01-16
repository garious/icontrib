function mapObject(input, output, func) {
    for(var prop in input) {
        if(typeof(input[prop]) == "object") {
            output[prop] = {};
            mapObject(input[prop], output[prop], func);
        } else {
            output[prop] = func(prop, input[prop]);
        }
    }
}

function listObject(input, output, func) {
    for(var prop in input) {
        if(typeof(input[prop]) == "object") {
            listObject(input[prop], output, func);
        } else {
            func(prop, input[prop], output);
        }
    }
}

function servedToFormInputs(name, val) {
    return E.input({type: "text", name: prop, size: "10", value: val});
}

function populateFromFormInputs(name, val, arr) {
    arr.push(name);
    arr.push(val);
}

var deps = [
    '/tag/tag.js', 
    '/jquery/jquery-mod.js'
];

function onReady(E, $) {
    var jsonForm = function(post_url, get_url) {
        var errorBox = E.div();
        var formInputs = {};
        var formArr = [];
        var jsonInfoBox = E.form(formArr);
        $.ajax({
            type: "GET",
            url: get_url,
            dataType: "json",
            success: function(data) {
                if(data.Right) {
                    mapObject(data.Right, formInputs, servedToFormInputs);
                    listObject(formInputs, formArr, populateFromFormInputs);
                } else if(data.Left) {
                    errorBox.innerHTML = data.Left;
                }
            }
        });
        jsonInfoBox.onsubmit = function(e){
            e.preventDefault();
            var data = {};
            mapObject(formInputs, data, function(name,val) { return val.value; } );
            $.ajax({
                type: "POST",
                url: post_url,
                data: JSON.stringify(data),
                dataType: "json",
                success: function(data) {
                    if(data.Left) {
                        errorBox.innerHTML = data.Left;
                    }
                }
            });
        };
        return jsonInfoBox;
    };
    return {
        jsonForm: jsonForm
    };
}

define(deps, onReady);

