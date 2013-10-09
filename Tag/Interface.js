//
// Interface-oriented programming for JavaScript
//

// Prototype:
//    getInterface(obj : {}, iid : String) : {Function(obj, ...)}
//
function getInterface(obj, iid) {
    var x = obj.constructor.interfaces;
    return x && x[iid];
}

function supportsInterface(o, i) {
    if (typeof i !== 'object') {
        return false;
    }
    for (var k in i) {
        var f = o[k];
        var v = i[k];
        if (typeof f !== 'function' || (typeof v === 'function' && f.length !== v.length)) {
            return false;
        }
    }
    return true;
}

//  //
//  // Create an object that implements only this interface
//  //
//  // Note: This is considerably more expensive than using the interface implementation directly
//  //
//
//  function bind(f, o) {
//      return function () {
//          return f(o, arguments);
//      };
//  }
//
//  function bindInterface(obj, iface) {
//      var ifaceObj = {};
//      for (var k in iface) {
//          if (iface.hasOwnProperty(k)) {
//              ifaceObj[k] = bind(iface[k], obj);
//          }
//      }
//      return ifaceObj;
//  }
//  
//  //
//  // If this object implements the requested interface, return an object that implements only that interface.
//  //
//  function queryInterface(obj, iid) {
//       var x = getInterface(obj, iid);
//       return x && bindInterface(obj, x);
//  }


Yoink.define({
    getInterface: getInterface,
    supportsInterface: supportsInterface
});
