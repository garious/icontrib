var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/skin/frame.js',
    '/skin/core.js',
    '/charity/popular.json', 
    '/skin/donor.js',
    '/donor/' + yoink.params.id + '.json'
];

function onReady(auth, tag, layout, frame, core, popularCharities, donor, user) {

    var box = core.box({
        width: 600,
        contents: donor.profile({user: user})
    });

    var body = layout.hug([
        box,
        layout.pillow(20),
        donor.recommendedFunds({funds: popularCharities})
    ]);

    yoink.define( frame.frame({contents: body, auth: auth}) );

}

yoink.require(deps, onReady);

