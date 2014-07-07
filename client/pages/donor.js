var deps = [
    '/donor/checkUser.json',
    '/stdlib/tag.js',
    '/stdlib/layout.js',
    '/skin/frame.js',
    '/skin/core.js',
    '/charity/popular.json',
    '/skin/donor.js',
    '/static/donor/' + yoink.params.id + '.json'
];

function onReady(auth, tag, layout, frame, core, popularCharities, donor, user) {

    var box = core.box({
        width: 600,
        contents: donor.profile({user: user})
    });

    var body = layout.hcat([
        box,
        layout.gap(20),
        donor.recommendedFunds({funds: popularCharities})
    ]);

    yoink.define( frame.frame({contents: body, auth: auth}) );

}

yoink.require(deps, onReady);

