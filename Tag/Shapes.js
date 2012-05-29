//
// A library for drawing simple shapes
//

var deps = [
    'Tag.js'
];

function onReady(Tag) {

    // Draw a circle using CSS
    function circle(as) {
        var r = as.radius;
        return Tag.tag({
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
    
    Yoink.define({
        circle: circle
    });

}

Yoink.require(deps, onReady);

