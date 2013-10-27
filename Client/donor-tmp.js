var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js',
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

