var deps = [
    'frame.js',
    'core.js'
];

function onReady(frame, core) {

    var node = frame.footer({style: {backgroundColor: '#EEE'}}, [
        core.hyperlink({url: '#', text: 'blah'})
    ]);

    define(node);
}

require(deps, onReady);

