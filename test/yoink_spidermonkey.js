if (typeof XMLHttpRequest == 'undefined') {
    var XMLHttpRequest = function() {
       this.open = function(method, url, b) { this.responseText = snarf(url); };
       this.send = function(){};
    };
}

eval( snarf('../yoink.js') );

