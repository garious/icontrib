// Definition of the 2D interface ID

var deps = [
    'Tag.js'
];

function onReady(Tag) {

    var twoDimensionalId = Yoink.fileUrl;
    
    // TwoDimensional instance for Tag objects
    Tag.tag.interfaces[twoDimensionalId] = {

        setPosition: function (me, pos) {

            if (!me.attributes) {
                me.attributes = {};
            }

            if (!me.style) {
                me.style = {};
            }

            Tag.mixin(me.style, pos);
    
            return me;
        }

    };
    
    Yoink.define({
        twoDimensionalId: twoDimensionalId
    });
}

Yoink.require(deps, onReady);

