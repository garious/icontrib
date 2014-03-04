//
// Layout tests with hugging and spooning
//

var deps = [
    'tag.js',
    'layout.js'
];

function onReady(tag, layout) {

    function testImg() {
        return tag.tag({name: 'img', attributes: {src: 'logo.png'}, style: {border: '1px solid', padding: '5px', borderRadius: '5px'}});
    }

    function hugTest() {
        return layout.hug([
            tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'hello'}), 
            layout.pillow(10),
            tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'world'})
        ]);
    }

    function test() {
        var separator = layout.pillow(30);

        function label(s, e) {
            return layout.hug([tag.tag({name: 'p', style: {width: '70px'}, contents: s}), layout.pillow(10), e]);
        }

        return layout.hug([
            layout.pillow(10),
            layout.spoon([
                layout.pillow(10),
                layout.spoon([
                    label('hug',   layout.hug(  [testImg(), layout.pillow(10), testImg(), layout.pillow(10), testImg()])), separator,
                    label('kiss',  layout.hug(  [testImg(), testImg(),         testImg()])), separator,
                    label('spoon', layout.spoon([testImg(), layout.pillow(10), testImg(), layout.pillow(10), testImg()])), separator,
                    label('love',  layout.spoon([testImg(), testImg(),         testImg()]))
                ]),
                layout.pillow(10) 
            ]),
            layout.pillow(10)
        ]);
    }

    yoink.define(test());
}


yoink.require(deps, onReady);

