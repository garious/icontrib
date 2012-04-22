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
        return Tag.tag('div', {
            style: {
                left: (as.left || 0) + 'px',
                right: (as.right || 0) + 'px',
                top: (as.top || 0) + 'px',
                bottom: (as.bottom || 0) + 'px',
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
    
    define({
        circle: circle
    });

}

require(deps, onReady);

