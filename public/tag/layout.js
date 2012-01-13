//
// Layout with hugs, kisses, spooning, and lovemaking
//

// All combinators are of type "Array a -> a"
//
// kiss( ['a','b','c']) === 'abc'
// hug(  ['a','b','c']) === 'a b c'
// love( ['a','b','c']) === 'a\nb\nc'
// spoon(['a','b','c']) === 'a\n\nb\n\nc'

// Limitations:
//    * Must know the size of each element before it is rendered.
//

var deps = [
    'tag.js'
];

// 'getStyle' yoinked from John Resig's "Pro JavaScript Techniques"
function getStyle(elem, name) {
    if (elem.style[name]) {
        return elem.style[name];
    } else if (elem.currentStyle) {
        return elem.currentStyle[name];
    } else if (document.defaultView && document.defaultView.getComputedStyle) {
        name = name.replace(/([A-Z])/g,'-$1');
        name = name.toLowerCase();
        var s = document.defaultView.getComputedStyle(elem, '');
        return s && s.getPropertyValue(name);
    }
}

function onReady(E) {
    var defaultPadding = 10;

    function hug(xs, pad) {
        if (pad === undefined) {
            pad = defaultPadding;
        }
        var height = 0;
        var width = 0;
        for (var i = 0; i < xs.length; i++) {
           var x = xs[i];
           var h, w = 0;
           x.style.position = 'absolute';
           x.style.left = width + 'px';
           x.style.top = '0px';
           h = parseInt(getStyle(x, 'height')) || 0;
           w = parseInt(getStyle(x, 'width')) || 0;
           width = width + pad + w;
           height = h > height ? h : height;
        }
        width = xs.length > 0 ? width - pad : 0;
        return E.div({style: {height: height, width: width}}, xs);
    }

    function kiss(xs) {
        return hug(xs, 0);
    }
    
    function spoon(xs, pad) {
        if (pad === undefined) {
            pad = defaultPadding;
        }
        var height = 0;
        var width = 0;
        for (var i = 0; i < xs.length; i++) {
           var x = xs[i];
           var h, w = 0;
           x.style.position = 'absolute';
           x.style.left = '0px';
           x.style.top = height + 'px';
           h = parseInt(getStyle(x, 'height')) || 0;
           w = parseInt(getStyle(x, 'width')) || 0;
           height = height + pad + h;
           width = w > width ? w : width;
        }
        height = xs.length > 0 ? height - pad : 0;
        return E.div({style: {height: height, width: width}}, xs);
    }

    function love(xs) {
        return spoon(xs, 0);
    }

    function space(w, h) {
        return E.div({style: {height: h, width: w}}, []);
    };


    function label(s, e) {
        return hug([E.div({style: {height: '20px', width: '70px'}}, [s]), e]);
    }

    function testImg() {
        return E.img({border: 1, src: '/images/logo.png', style: {height: '90px', width: '100px', borderRadius: '5px'}});
    }

    function test() {
        return kiss([
            space(defaultPadding, defaultPadding), 
            love([
                space(defaultPadding, defaultPadding), 
                spoon([
                    label('hug',   hug(  [testImg(), testImg(), testImg()])),
                    label('kiss',  kiss( [testImg(), testImg(), testImg()])),
                    label('spoon', spoon([testImg(), testImg(), testImg()])),
                    label('love',  love( [testImg(), testImg(), testImg()]))
                ], 30),
            ]),
        ]);
    }

    return {
        hug: hug,
        kiss: kiss,
        spoon: spoon,
        love: love,
        space: space,
        label: label,
        testImg: testImg,
        test: test
    };
}


return {
    deps: deps,
    callback: onReady
};


