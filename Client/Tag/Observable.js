var deps = [
    'Interface.js'
];


//
// Observable JS
//

var observableId = Yoink.fileUrl;

function onReady(Iface) {

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

        return me;
    }
    
    observe.interfaces = {};
    observe.interfaces[observableId] = {
        get: function(me) {
            return me.value;
        },
        subscribe: function(me, f) {
            if (!me.subscribers) {
                me.subscribers = [f];
            } else {
                me.subscribers.push(f);
            }
            return me;
        }
    };


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

        xs.forEach(function(o) {
            var methods = Iface.getInterface(o, observableId);
            if (methods) {
                methods.subscribe(o, function (val, obs) {
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
    
    thunk.interfaces = {};
    thunk.interfaces[observableId] = {
        get: function(me) {
           if (me.valid) {
             return me.value;
           } else {
             var vals = me.publishers.map(function(o){
                 var methods = Iface.getInterface(o, observableId);
                 if (methods) {
                     return methods.get(o);
                 } else {
                     return o;
                 }
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
        },
        subscribe: function(me, f) {
            if (!me.subscribers) {
                me.subscribers = [f];
            } else {
                me.subscribers.push(f);
            }
            return me;
        }
    };
    
    
    // Handy function to lift a raw function into the observable realm
    function lift(f) {
      return function() {
         var args = Array.prototype.slice.call(arguments);
         return thunk(args, f);
      };
    }

    Yoink.define({
        observableId: observableId,
        observe: observe,
        thunk: thunk,
        lift: lift
    });

}

Yoink.require(deps, onReady);

