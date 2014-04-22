//
// 2D Layout tests
//

var deps = [
    'tag.js',
    'layout.js'
];

function onReady(tag, layout) {

    function testImg() {
        return tag.tag({name: 'h3', contents: 'text'});
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

