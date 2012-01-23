var deps = [
    '/tag/tag.js', 
    '/jquery/jquery-mod.js',
    '/jsonform/jsonform.js'
];

function onReady(E, $, JF) { 

    var loginCtor = function(cfg) {
        var logoutUrl = cfg.root + "/logout";
        var loginUrl  = cfg.root + "/login";
        var checkUrl  = cfg.root + "/check";
        var addUrl    = cfg.root + "/add";
        //stupid schema, the 'null' services as a sentinal when i traverse it
        var schema = {
            UserLogin: {
                email: null,
                password: null
            }
        };
        var inputs = JF.map(schema, schema, {}, JF.toInput);
        var loginSubmit = E.input({type: 'submit', value: 'Log in'});
        
        var logout = E.input({type: 'submit', value : 'Logout' });
        var logoutForm = E.form([logout]);
        $(logoutForm).submit( function(e) {
            e.preventDefault();
            $.ajax({
                type: "GET",
                url: logoutUrl,
                success: function(data) {
                    window.location.reload();
                }
            });
        });
        var stylize = function(name, val) {
            val.type = 'text';
            val.name = name;
            val.size = 10;
            return val;
        };
        //now i have an object with a bunch of empty inputs, whose layout matches my schema
        //i can traverse the schema in parallel with the object and reference the input fields
        inputs = JF.map(schema, inputs, inputs, stylize);
        inputs.UserLogin.password.type = 'password';
        var loginForm = E.form([
            inputs.UserLogin.email, 
            inputs.UserLogin.password, 
            loginSubmit
        ]);
        var widget = E.div([logoutForm]);

        $(loginForm).submit( function(e) {
            e.preventDefault();
            var formValues = JF.map(schema, inputs, {}, JF.toVal);
            var dataString = JSON.stringify(formValues);
            $.ajax({
                type: "POST",
                url: loginUrl,
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
            url: checkUrl,
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
    define({
        loginForm: loginCtor
    });
}

require(deps, onReady);

