//
// Interface-oriented programming for JavaScript
//
function getInterface(obj, iid) {
     return obj.constructor.interfaces && obj.constructor.interfaces[iid];
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


define({
    getInterface: getInterface
});
