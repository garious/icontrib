return YOINK.module([

    '../tag/tag.js', 
    'indexbody.html',

], function(E, garbage) { 

    var body = function() {
        var div = E.div();
        div.innerHTML = garbage;
        return div;
    };

    return {
        body: body,
    };
});

