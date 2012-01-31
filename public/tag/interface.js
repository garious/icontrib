//
// Interface-oriented programming for JavaScript
//
function getInterface(obj, iface) {
     var ifaces = obj && obj.constructor && obj.constructor.interfaces;
     if (ifaces) {
         for (var i = 0; i < ifaces.length; i += 1) {
             var x = ifaces[i];
             if (x['interface'] === iface) {
                 return x.instance;
             }
         }
     }
     return undefined;
}

//  //
//  // Create an object that implements only this interface
//  //
//  // Note: This is considerably more expensive than using the interface implementation directly
//  //
//  function bindInterface(obj, iface) {
//      var ifaceObj = {};
//      for (var k in iface) {
//          if (iface.hasOwnProperty(k)) {
//              ifaceObj[k] = function () {
//                  return iface[k](obj, arguments);
//              };
//          }
//      }
//      return ifaceObj;
//  }
//  
//  //
//  // If this object implements the requested interface, return an object that implements only that interface.
//  //
//  function queryInterface(obj, iface) {
//       var x = getInterface(obj, iface);
//       return x && bindInterface(obj, x);
//  }


define({
    getInterface: getInterface
});
