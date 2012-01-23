var deps = [
    '/tag/tag.js', 
    '/jquery/jquery-mod.js',
    '/jsonform/jsonform.js'
];

function onReady(E, $) {
    var signup = function (cfg) {
        $(cfg.form).submit(function (e) {
            e.preventDefault();
            var dataString = $(form).serialize();
            $.ajax({
                type: "POST",
                url: cfg.root + '/update',
                data: dataString,
                dataType: "json",
                success: function(data) {
                    var dataString = JSON.stringify(data);
                    console.log(dataString);
                }
             });
        });
    };
    return {
        signup: signup
    };
}

define(deps, onReady);

