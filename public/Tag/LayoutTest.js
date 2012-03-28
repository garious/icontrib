//
// Layout tests with hugging and spooning
//

var deps = [
    'Tag.js',
    'Layout.js'
];

function onReady(Tag, Layout) {

    function testImg() {
        return Tag.img({src: '/Skin/logo.png', style: {border: 'solid 1px', padding: '5px', borderRadius: '5px', width: '112px', height: '100px'}});
    }

    function hugTest() {
        return Layout.hug([
            Tag.span({style: {height: '20px', width: '70px'}}, 'hello'), 
            Layout.pillow(10),
            Tag.span({style: {height: '20px', width: '70px'}}, 'world')
        ]);
    }

    function test() {
        var pad = Layout.pillow(10);
        var separator = Layout.pillow(30);

        function label(s, e) {
            return Layout.hug([Tag.p({style: {width: '70px'}}, s), pad, e]);
        }

        return Layout.hug([
            pad,
            Layout.spoon([
                pad,
                Layout.spoon([
                    label('hug',   Layout.hug(  [testImg(), pad, testImg(), pad, testImg()])), separator,
                    label('kiss',  Layout.hug(  [testImg(),      testImg(),      testImg()])), separator,
                    label('spoon', Layout.spoon([testImg(), pad, testImg(), pad, testImg()])), separator,
                    label('love',  Layout.spoon([testImg(),      testImg(),      testImg()]))
                ]),
                pad 
            ]),
            pad
        ]);
    }

    define(test());
}


require(deps, onReady);

