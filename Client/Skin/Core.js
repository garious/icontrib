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
            color: 'blue'
        };

        var handlers = {
            mouseover: function(evt) { evt.target.style.textDecoration = 'underline'; },
            mouseout:  function(evt) { evt.target.style.textDecoration = 'none'; }
        };

        return Tag.tag('a', {style: sty, href: as.url}, [as.text], handlers);
    }

    function image(as) {
        var sty = {
            width:  as.width  && as.width  + 'px',
            height: as.height && as.height + 'px',
            borderRadius: as.borderRadius && as.borderRadius + 'px'
        };

        return Tag.tag('img', {style: sty, src: as.url, alt: as.text}, null, {click: as.onClick});
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
            size: as.size,
            placeholder: as.placeholder || '',
            style: sty
        };

        // Special handling for 'value' attribute, which will awkwardly write the text "undefined".
        if (as.value !== undefined) {
            attrs.value = as.value;
        }

        var handlers = {keyup: as.onKeyUp, change: as.onChange};

        return Tag.tag('input', attrs, null, handlers);
    }

    function button(as) {
        var color      = as.loud ? Colors.red : Colors.gray;
        var focusColor = as.loud ? Colors.red : Colors.lightColor;

        var handlers = {
            mouseover: function(evt) { evt.target.style.backgroundColor = focusColor; },
            mouseout:  function(evt) { evt.target.style.backgroundColor = color; },
            click: as.onClick
        };

        return Tag.tag('a', {
            href: as.href || '#', 
            style: {
                font: font, 
                textDecoration: 'none', 
                textAlign: 'center', 
                backgroundColor: color, 
                color: '#fff', 
                padding: '5px', 
                borderRadius: '2px'
            }
        }, as.text, handlers);
    }

    function box(as) {
        var shadow = '0px 0px 5px 2px #ddd';
        var e = as.contents;

        var padding = 15;
        return Tag.tag('div', {
            style: {
                'float': 'left',
                border: '2px solid #cfcfcf',
                shadow: shadow,
                MozBoxShadow: shadow,
                WebkitBoxShadow: shadow,
                width:  as.width && (as.width - 2 * padding - 4) + 'px',
                height: as.height && (as.height - 2 * padding - 4) + 'px',
                padding: padding + 'px'
            }
        }, [e], {
            keyup: as.onKeyUp
        });
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

            var sty = hStyle({fontSize: Colors['h' + n + 'Size'], maxWidth: as.maxWidth}, s); 

            if (typeof as === 'object') {
                sty.color = as.color !== undefined ? as.color : sty.color;
            }

            return Tag.tag('h' + n, {style: sty}, s);
        }

        return header;
    }

    function label(s) {
        return Tag.tag('label', {style: {font: font}}, s);
    }

    function p(as) {
        if (typeof as === 'string') {
            as = {text: as};
        }
        return Tag.tag('p', {style: {font: font, margin: '0px'}}, as.text);
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
           backgroundColor: as.color
        };

        return Tag.tag('hr', {style: sty, noshade: true, size: 1});
    }

    define({
         hyperlink: hyperlink,
         image: image,
         input: input,
         label: label,
         button: button,
         box: box,
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

require(deps, onReady);

