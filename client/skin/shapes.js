//
// A library for drawing simple shapes
//

var deps = [
    '/yoink/tag.js'
];

function onReady(tag) {

    // Draw a circle using CSS
    function circle(as) {
        var r = as.radius;
        return tag.tag({
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
    
    yoink.define({
        circle: circle
    });

}

yoink.require(deps, onReady);

