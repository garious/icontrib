var deps = [
    'Frame.js',
    'Core.js'
];

function onReady(Frame, Core) {

    var node = Frame.footer({style: {backgroundColor: '#EEE'}}, [
        Core.hyperlink({url: '#', text: 'blah'})
    ]);

    define(node);
}

require(deps, onReady);

