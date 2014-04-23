//
// 2D Layout tests
//

var deps = [
    'yoink/tag.js',
    'yoink/layout.js'
];

function onReady(tag, layout) {

    function testImg() {
        return tag.tag({name: 'img', attributes: {src: 'logo.png'}, style: {border: '1px solid', padding: '5px', borderRadius: '5px'}});
    }

    function hcatTest() {
        return layout.hcat([
            tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'hello'}), 
            layout.gap(10),
            tag.tag({name: 'span', style: {height: '20px', width: '70px'}, contents: 'world'})
        ]);
    }

    function test() {
        var separator = layout.gap(30);

        function label(s, e) {
            return layout.hcat([tag.tag({name: 'p', style: {width: '70px'}, contents: s}), layout.gap(10), e]);
        }

        return layout.hcat([
            layout.gap(10),
            layout.vcat([
                layout.gap(10),
                layout.vcat([
                    label('hcat', layout.hcat([testImg(), layout.gap(10), testImg(), layout.gap(10), testImg()])), separator,
                    label('vcat', layout.vcat([testImg(), layout.gap(10), testImg(), layout.gap(10), testImg()])), separator,
                ]),
                layout.gap(10)
            ]),
            layout.gap(10)
        ]);
    }

    yoink.define(test());
}


yoink.require(deps, onReady);

