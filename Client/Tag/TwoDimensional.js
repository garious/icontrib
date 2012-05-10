// Definition of the 2D interface ID

var deps = [
    'Tag.js'
];

function onReady(Tag) {

    var twoDimensionalId = Yoink.baseUrl + '/TwoDimensional.js';
    
    // TwoDimensional instance for Tag objects
    Tag.tag.interfaces[twoDimensionalId] = {

        setPosition: function (me, pos) {

            if (!me.attributes) {
                me.attributes = {};
            }

            if (!me.style) {
                me.style = {};
            }

            var sty = me.style;

            if (pos['float'] !== undefined) {
                sty['float'] = pos['float'];
            }

            if (pos.clear !== undefined) {
                sty.clear = pos.clear;
            }
    
            return me;
        }

    };
    
    Yoink.define({
        twoDimensionalId: twoDimensionalId
    });
}

Yoink.require(deps, onReady);

