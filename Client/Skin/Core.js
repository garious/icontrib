var deps = [
    '/Tag/Tag.js',
    'Colors.js'
];

function onReady(Tag, Colors) {

    var defaultFont = "/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";
    var defaultFontSize = 15;
    var font = defaultFontSize + "px" + defaultFont;

    function hyperlink(as) {
        var sty = {
            textDecoration: 'none',
            font: font,
            color: 'blue',
            marginLeft:   as.marginLeft   ? as.marginLeft   + 'px' : 0,
            marginRight:  as.marginRight  ? as.marginRight  + 'px' : 0,
            marginTop:    as.marginTop    ? as.marginTop    + 'px' : 0,
            marginBottom: as.marginBottom ? as.marginBottom + 'px' : 0
        };

        var handlers = {
            mouseover: function(evt) { evt.target.style.textDecoration = 'underline'; },
            mouseout:  function(evt) { evt.target.style.textDecoration = 'none'; }
        };

        return Tag.tag({
            name: 'a',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            attributes: {href: as.url},
            contents: [as.text],
            handlers: handlers
        });
    }

    function image(as) {
        var sty = {
            width:  as.width  && as.width  + 'px',
            height: as.height && as.height + 'px',
            borderWidth: '0px',
            borderRadius: as.borderRadius && as.borderRadius + 'px'
        };

        return Tag.tag({
            name: 'img',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            attributes: {src: as.url, alt: as.text},
            handlers: {click: as.onClick}
        });
    }

    function input(as) {
        var width  = (as.width  || as.size * 10) + 'px';
        var height = (as.height || 30) + 'px';

        var sty = {
            height: height,
            width: width,
            border: '2px solid',
            borderColor: Colors.gray,
            borderRadius: '2px',
            font: font,
            paddingLeft: '10px',
            paddingRight: '10px'
        };

        var attrs = {
            type: as.type,
            disabled: as.disabled,
            size: as.size,
            autofocus: as.autofocus,
            placeholder: as.placeholder || ''
        };

        // Special handling for 'value' attribute, which will awkwardly write the text "undefined".
        if (as.value !== undefined) {
            attrs.value = as.value;
        }

        var handlers = {keyup: as.onKeyUp, change: as.onChange};

        return Tag.tag({
            name: 'input',
            attributes: attrs,
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            handlers: handlers
        });
    }

    function button(as) {
        var color      = as.loud ? Colors.red        : as.quiet ? Colors.gray       : Colors.green;
        var focusColor = as.loud ? Colors.lightColor : as.quiet ? Colors.lightColor : Colors.lightGreen;

        var handlers = {
            mouseover: function(evt) { evt.target.style.backgroundColor = focusColor; },
            mouseout:  function(evt) { evt.target.style.backgroundColor = color; },
            click: as.onClick
        };

        var sty = {
            font: font, 
            width:  as.width ? as.width + 'px' : undefined,
            textDecoration: 'none', 
            textAlign: 'center', 
            backgroundColor: color, 
            color: '#fff', 
            padding: '5px', 
            borderRadius: '2px'
        };

        return Tag.tag({
            name: 'a',
            attributes: {href: as.href || '#'},
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            contents: as.text,
            handlers: handlers
        });
    }

    function box(as) {
        var shadow = '0px 0px 5px 2px #ddd';
        var padding = 15;
        var e = as.contents;

        var sty = {
            cssFloat: 'left',
            border: '2px solid #cfcfcf',
            shadow: shadow,
            MozBoxShadow: shadow,
            WebkitBoxShadow: shadow,
            width:  as.width && (as.width - 2 * padding - 4) + 'px',
            height: as.height && (as.height - 2 * padding - 4) + 'px',
            padding: padding + 'px'
        };

        return Tag.tag({
            name: 'div',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            contents: [e],
            handlers: {keyup: as.onKeyUp}
        });
    }

    function menu(as) {

        var width = as.width;

        var listItems = [];

        for (var i = 0; i < as.menuItems.length; i += 1) {
            if (i !== 0) {
                listItems.push( hr({width: width}) );
            }
            listItems.push(as.menuItems[i]);
        }

        var sty = {
            visibility: as.visibility,
            border: '1px solid',
            borderColor: Colors.lightColor,
            borderRadius: '0px 0px 5px 5px',
            width:  as.width + 'px',
            position: 'absolute',
            top: as.top + 'px',
            right: '0px',
            backgroundColor: '#eee',
            zIndex: 1
        };

        return Tag.tag({
            name: 'div',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            contents: listItems
        });
    }

    function mkRedirect(url) {
        return function() {
             window.location = url;
        };
    }

    function menuItem(as) {
        var onSelect = typeof as.onSelect === 'string' ? mkRedirect(as.onSelect) : as.onSelect;

        var sty = {
            width: '100%',
            padding: '10px'
        };

        var div = Tag.tag({
            name: 'div',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            contents: [as.contents],
            handlers: {click: onSelect}
        });
        return Tag.tag({name: 'a', attributes: {href: '#'}, style: {textDecoration: 'none'}, contents: [div]});
    }

    // Create the style attribute for HTML header elements
    function hStyle(as, s) {

        return {
            font: font,
            fontSize: as.fontSize + 'px',
            margin: 0,
            color: Colors.darkColor
        };
    }

    // Create a header constructor.  
    //
    // The returned constructor accepts either an attributes object or a string.
    //
    //     mkHeader(2)('hello!') === h2('hello!')
    //     mkHeader(3)('hello!') === h3('hello!')
    //     mkHeader(3)('hello!') === h3({text: 'hello!'})
    //
    function mkHeader(n) {

        function header(as) {
            var s = typeof as === 'string' ? as : as.text;

            var sty = hStyle({fontSize: Colors['h' + n + 'Size']}, s); 

            if (typeof as === 'object') {
                sty.color = as.color !== undefined ? as.color : sty.color;
            }

            return Tag.tag({
                name: 'h' + n,
                style: as.style ? Tag.mixin(sty, as.style) : sty,
                contents: s
            });
        }

        return header;
    }

    function label(s) {
        return Tag.tag({name: 'label', style: {font: font}, contents: s});
    }

    function p(as) {
        if (typeof as === 'string') {
            as = {text: as};
        }

        var sty = {font: font, margin: '0px', width: as.width && as.width + 'px'};

        return Tag.tag({
            name: 'p',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            contents: as.text
        });
    }

    function hr(as) {
        as = as || {};

        var sty = {
            height: as.height ? as.height + 'px' : '1px',
            width:  as.width  ? as.width  + 'px' : '100%',
            margin: as.margin ? as.margin + 'px' : 0,
            marginLeft:   as.marginLeft   ? as.marginLeft   + 'px' : 0,
            marginRight:  as.marginRight  ? as.marginRight  + 'px' : 0,
            marginTop:    as.marginTop    ? as.marginTop    + 'px' : 0,
            marginBottom: as.marginBottom ? as.marginBottom + 'px' : 0,
            borderWidth: 0,
            backgroundColor: as.color,
            clear: 'both'     // Required by Firefox and Opera
        };

        return Tag.tag({
            name: 'hr',
            style: as.style ? Tag.mixin(sty, as.style) : sty,
            attributes: {noshade: true, size: 1}
        });
    }

    Yoink.define({
         hyperlink: hyperlink,
         image: image,
         input: input,
         label: label,
         button: button,
         box: box,
         menu: menu,
         menuItem: menuItem,
         h1: mkHeader(1),
         h2: mkHeader(2),
         h3: mkHeader(3),
         h4: mkHeader(4),
         h5: mkHeader(5),
         h6: mkHeader(6),
         p: p,
         hr: hr,
         defaultFont: font
    });
}

Yoink.require(deps, onReady);

