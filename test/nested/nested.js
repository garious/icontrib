return {
    deps: ['../assert.js', 'neighbor.js'],
    callback: function(a, msg) {
        a.assert(msg == 'hello neighbor');
    }
};


