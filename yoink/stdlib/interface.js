//
// Interface-oriented programming for JavaScript
//

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

yoink.define({
    supportsInterface: supportsInterface
});
