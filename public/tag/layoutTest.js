//
// Layout tests with hugging and spooning
//

var deps = [
    'tag.js',
    'layout.js'
];

function onReady(E, L) {

    function testImg() {
        return E.img({border: 1, src: '/images/logo.png', style: {borderRadius: '5px'}});
    }

    function hugTest() {
        return L.hug([E.p('hello'), E.p('world')], 10);
    }

    function test() {
        var pad = 10;

        function label(s, e) {
            return L.hug([E.p({style: {width: '70px'}}, s), e], pad);
        }

        return L.hug([
            L.pillow(pad),
            L.spoon([
                L.pillow(pad),
                L.spoon([
                    label('hug',   L.hug(  [testImg(), testImg(), testImg()], pad)),
                    label('kiss',  L.hug(  [testImg(), testImg(), testImg()]     )),
                    label('spoon', L.spoon([testImg(), testImg(), testImg()], pad)),
                    label('love',  L.spoon([testImg(), testImg(), testImg()]     ))
                ], 30)
            ])
        ]);
    }

    return {
        main: test,
        hugTest: hugTest
    };
}


define(deps, onReady);

