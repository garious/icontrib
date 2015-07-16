//
// A library for drawing simple shapes
//

var dom = require('poochie/dom');

// Draw a circle using CSS
function circle(as) {
    var r = as.radius;
    return dom.element({
        name: 'div',
        style: {
            left: as.left,
            right: as.right,
            top: as.top,
            bottom: as.bottom,
            position: 'relative',
            width: 2 * r + 'px',
            height: 2 * r + 'px',
            background: as.color,
            mozBorderRadius: r + 'px',
            webkitBorderRadius: r + 'px',
            borderRadius: r + 'px'
        }
    });
}

module.exports = {
    circle: circle
};
