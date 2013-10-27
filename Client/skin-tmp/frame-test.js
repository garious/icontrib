var deps = [
    'Frame.js',
    'Core.js'
];

function onReady(frame, core) {

    var node = frame.footer({style: {backgroundColor: '#EEE'}}, [
        core.hyperlink({url: '#', text: 'blah'})
    ]);

    yoink.define(node);
}

yoink.require(deps, onReady);

