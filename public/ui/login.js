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
            email: null,
            password: null
        };
        var swapNode = function(newNode, oldNode) {
            oldNode.parentNode.replaceChild(newNode, oldNode);
            return newNode;
        };
        var inputs = JF.map(schema, schema, {}, JF.toInput);
        var loginSubmit = E.input({type: 'submit', value: 'Log in'});
        var register = E.input({type: 'submit', value: 'Register'});
        
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
        var stylize = function(name, val, oval) {
            val.type = 'text';
            val.name = name;
            val.size = 10;
            return val;
        };
        //now i have an object with a bunch of empty inputs, whose layout matches my schema
        //i can traverse the schema in parallel with the object and reference the input fields
        inputs = JF.map(schema, inputs, inputs, stylize);
        inputs.password.type = 'password';
        var loginForm = E.form([
            inputs.email, 
            inputs.password, 
            loginSubmit
        ]);
        var widget = E.div([
             logoutForm
        ]);

        $(loginForm).submit( function(e) {
            e.preventDefault();
            var formValues = JF.map(schema, inputs, {}, JF.toVal);
            var dataString = JSON.stringify(formValues);
    console.log(formValues);
            $.ajax({
                type: "POST",
                url: loginUrl,
                data: dataString,
                dataType: "json",
                success: function(data) {
                    if(data.Left) {
                        loginSubmit = swapNode(E.input({type: 'submit', value: 'Try Again'}), loginSubmit);
                    } else {
                        var logoutNew = E.input({type: 'submit', value : 'Logout ' + data.Right });
                        logout = swapNode(logoutNew, logout);
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
                    logout = swapNode(logoutNew, logout);
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
