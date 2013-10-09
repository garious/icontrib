//
// Observable JS
//

var observableId = {
    get:       function() {},
    subscribe: function(f) {}
};

// Observable values
function observe(v) {
    var me = {
        value: v,
        constructor: observe
    };

    me.set = function(v) {
        me.value = v;
        if (me.subscribers) {
            me.subscribers.forEach(function(f) {
                f(me);
            });
        }
        return me;
    };

    me.get = function() {
        return me.value;
    };

    me.subscribe = function(f) {
        if (!me.subscribers) {
            me.subscribers = [f];
        } else {
            me.subscribers.push(f);
        }
        return me;
    };

    return me;
}

// Observable computations.  thunk() takes a list of observables
// and a callback function and returns an observable.  Any time
// a value is requested AND an input has changed, the given callback
// is executed, and its return value is returned.
function thunk(xs, f) {
    var me = {
        valid: false,
        f: f,
        publishers: xs,
        constructor: thunk
    };

    me.get = function() {
       if (me.valid) {
         return me.value;
       } else {
         var vals = me.publishers.map(function(o){
             return o.get && o.subscribe ? o.get() : o;
         });

         var oldValue = me.value;
         me.value = me.f.apply(null, vals);
         me.valid = true;

         if (me.value !== oldValue && me.subscribers) {
             me.subscribers.forEach(function(f) {
                 f(me);
             });
         }

         return me.value;
       }
    };

    me.subscribe = function(f) {
        if (!me.subscribers) {
            me.subscribers = [f];
        } else {
            me.subscribers.push(f);
        }
        return me;
    };

    xs.forEach(function(o) {
        if (o.get && o.subscribe) {
            o.subscribe(function (val, obs) {
                if (me.valid) {
                    me.valid = false;
                    if (me.subscribers) {
                        me.subscribers.forEach(function(f) {
                            f(me);
                        });
                    }
                }
            });
        }
    });

    return me;
}

// Handy function to lift a raw function into the observable realm
function lift(f) {
    return function() {
       var args = Array.prototype.slice.call(arguments);
       return thunk(args, f);
    };
}


// Handy function to capture the current state of an object containing observables
function snapshot(o) {
    if (typeof o === 'object') {
        if (o.get && o.subscribe) {
            return snapshot( o.get() );
        } else {
            if (o.constructor === Array) {
                return o.map(snapshot);
            } else {
                var o2 = {};
                var k;
                for (k in o) {
                    if (o.hasOwnProperty(k)) {
                        o2[k] = snapshot(o[k]);
                    }
                }
                return o2;
            }
        }
    } else {
        return o;
    }
}

Yoink.define({
    observableId: observableId,
    observe: observe,
    thunk: thunk,
    lift: lift,
    snapshot: snapshot
});

