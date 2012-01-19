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
    '/tag/tag.js', 
    '/jquery/jquery-mod.js'
];

function onReady(E, $) { 

    var loginCtor = function(cfg) {
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
        var loginSubmit = E.input({type: 'submit', value: 'Log in'});
        formArr.push(loginSubmit);
        
        var logout = E.input({type: 'submit', value : 'Logout' });
        var logoutForm = E.form([logout]);
        $(logoutForm).submit( function(e) {
            e.preventDefault();
            $.ajax({
                type: "GET",
                url: cfg.logout,
                success: function(data) {
                    window.location.reload();
                }
            });
        });

        var loginForm = E.form(formArr);
        var widget = E.div([logoutForm]);

        $(loginForm).submit( function(e) {
            e.preventDefault();
            var toVal = function(name, val) {
                    return val.value;
                };
            var formValues = mapObject(formType, formInputs, {}, toVal);
            var dataString = JSON.stringify(formValues);
            $.ajax({
                type: "POST",
                url: cfg.login,
                data: dataString,
                dataType: "json",
                success: function(data) {
                    if(data.Left) {
                        var loginSubmitNew = E.input({type: 'submit', value: 'Try Again'});
                        loginForm.replaceChild(loginSubmitNew, loginSubmit); 
                        loginSubmit = loginSubmitNew;
                    } else {
                        var logoutNew = E.input({type: 'submit', value : 'Logout ' + data.Right });
                        logoutForm.replaceChild(logoutNew, logout); 
                        logout = logoutNew;
                        widget.replaceChild(logoutForm, loginForm); 
                    }
                }
            });
        });
        $.ajax({
            type: "GET",
            url: cfg.check,
            dataType: "json",
            success: function(data) {
                if(data.Right) {
                    var logoutNew = E.input({type: 'submit', value : 'Logout ' + data.Right });
                    logoutForm.replaceChild(logoutNew, logout); 
                    logout = logoutNew;
                } else {
                    widget.replaceChild(loginForm, logoutForm); 
                }
            }
        });
        return widget;
    };
    return {
        loginForm: loginCtor
    };
}

define(deps, onReady);

