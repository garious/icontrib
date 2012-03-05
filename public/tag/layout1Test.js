//
// Layout tests with hugging and spooning
//

var deps = [
    'tag.js',
    'layout1.js'
];

function onReady(E, L) {

    function testImg() {
        return E.img({border: 1, src: '/ui/logo.png', style: {borderRadius: '5px', width: '112px', height: '100px'}});
    }

    function hugTest() {
        return L.hug([
            E.span({style: {height: '20px', width: '70px'}}, 'hello'), 
            L.pillow(10),
            E.span({style: {height: '20px', width: '70px'}}, 'world')
        ]);
    }

    function test() {
        var pad = L.pillow(10);
        var separator = L.pillow(30);

        function label(s, e) {
            return L.hug([E.p({style: {width: '70px'}}, s), pad, e]);
        }

        return L.hug([
            pad,
            L.spoon([
                pad,
                L.spoon([
                    label('hug',   L.hug(  [testImg(), pad, testImg(), pad, testImg()])), separator,
                    label('kiss',  L.hug(  [testImg(),      testImg(),      testImg()])), separator,
                    label('spoon', L.spoon([testImg(), pad, testImg(), pad, testImg()])), separator,
                    label('love',  L.spoon([testImg(),      testImg(),      testImg()]))
                ]),
                pad 
            ]),
            pad
        ]);
    }

    define(test());
}


require(deps, onReady);

