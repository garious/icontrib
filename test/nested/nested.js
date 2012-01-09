return {
    deps: ['../assert.js', 'neighbor.js'],
    callback: function(a, msg) {
        a.assert(msg == 'hello neighbor');
        return {
            deps: ['../assert.js', 'neighbor.js'],
            callback: function(ass, sameMsg) {
                ass.assert(sameMsg == 'hello neighbor');
            }
        }
    }
};


