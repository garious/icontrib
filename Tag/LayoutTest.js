//
// Layout tests with hugging and spooning
//

var deps = [
    'Tag.js',
    'Layout.js'
];

function onReady(Tag, Layout) {

    function testImg() {
        return Tag.tag({name: 'img', attributes: {src: '/Skin/logo.png'}, style: {border: '1px solid', padding: '5px', borderRadius: '5px'}});
    }

    function hugTest() {
        return Layout.hug([
            Tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'hello'}), 
            Layout.pillow(10),
            Tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'world'})
        ]);
    }

    function test() {
        var separator = Layout.pillow(30);

        function label(s, e) {
            return Layout.hug([Tag.tag({name: 'p', style: {width: '70px'}, contents: s}), Layout.pillow(10), e]);
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

    Yoink.define(test());
}


Yoink.require(deps, onReady);

