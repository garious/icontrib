return {
    deps: [
        '../tag/tag.js', 
        'charitySignUpBody.html',
    ],

    callback: function(E, garbage) { 
    
        var body = function() {
            var div = E.div();
            div.innerHTML = garbage;
            return div;
        };
    
        return {
            body: body,
        };
    },
};

