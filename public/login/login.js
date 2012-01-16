function mapObject(type, input, output, func) {
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
}

function listObject(type, input, output, func) {
    for(var prop in type) {
        if(type.hasOwnProperty(prop)) {
            if(type[prop] === null) {
                func(prop, input[prop], output);
            } else if(typeof(type[prop]) == "object") {
                listObject(type[prop], input[prop], output, func);
            } 
        }
    }
    return output;
}


var deps = [
    '../tag/tag.js', 
    '../jquery/jquery-mod.js'
];

function onReady(E, $) { 

    var loginForm = function(loginUrl) {
        var toInput = function (name, val) {
                if(name == "password") {
                    return E.input({type: "password", name: name, size: "10"});
                } else {
                    if(val === null) {
                        return E.input({type: "text", name: name, size: "10"});
                    } else {
                        return E.input({type: "text", name: name, size: "10", value: val});
                    }
                }
            };
        var errorBox = E.div();
        var formType = {
            UserLogin: {
                email: null,
                password: null
            }
        };
        var formInputs = mapObject(formType, formType, {}, toInput);

        var toForm = function(name, val, arr) {
                arr.push(name);
                arr.push(val);
            };
        var formArr = listObject(formType, formInputs, [], toForm);
        formArr.push(E.input({type: 'submit', value: 'Log in'}));
        formArr.push(errorBox);

        var formBox = E.form(formArr);

        $(formBox).submit( function(e) {
            e.preventDefault();
            var toVal = function(name, val) {
                    return val.value;
                };
            var formValues = mapObject(formType, formInputs, {}, toVal);
            var dataString = JSON.stringify(formValues);
            $.ajax({
                type: "POST",
                url: loginUrl,
                data: dataString,
                dataType: "json",
                success: function(data) {
                    if(data.Left) {
                        errorBox.innerHTML = JSON.stringify(data);
                    } else {
                        window.location.reload();
                    }
                }
            });
        });

        return formBox;
    };
    return {
        loginForm: loginForm
    };
}

define(deps, onReady);

