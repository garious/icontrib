//
// Layout tests with hugging and spooning
//

var deps = [
    'Tag.js',
    'Layout.js'
];

function onReady(Tag, Layout) {

    function testImg() {
        return Tag.tag('img', {src: '/Skin/logo.png', style: {border: '1px solid', padding: '5px', borderRadius: '5px'}});
    }

    function hugTest() {
        return Layout.hug([
            Tag.tag('span', {style: {height: '20px', width: '70px'}}, 'hello'), 
            Layout.pillow(10),
            Tag.tag('span', {style: {height: '20px', width: '70px'}}, 'world')
        ]);
    }

    function test() {
        var separator = Layout.pillow(30);

        function label(s, e) {
            return Layout.hug([Tag.tag('p', {style: {width: '70px'}}, s), Layout.pillow(10), e]);
        }

        return Layout.hug([
            Layout.pillow(10),
            Layout.spoon([
                Layout.pillow(10),
                Layout.spoon([
                    label('hug',   Layout.hug(  [testImg(), Layout.pillow(10), testImg(), Layout.pillow(10), testImg()])), separator,
                    label('kiss',  Layout.hug(  [testImg(),      testImg(),      testImg()])), separator,
                    label('spoon', Layout.spoon([testImg(), Layout.pillow(10), testImg(), Layout.pillow(10), testImg()])), separator,
                    label('love',  Layout.spoon([testImg(),      testImg(),      testImg()]))
                ]),
                Layout.pillow(10) 
            ]),
            Layout.pillow(10)
        ]);
    }

    define(test());
}


require(deps, onReady);

