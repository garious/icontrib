var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js',
    '/stats/community.json'
];

function onReady(auth, tag, layout, frame, core, popularCharities, donor, community) {

    var body = layout.spoon([
        layout.hug([
            core.box({
                width: 600,
                contents: donor.profile({user: community})
            }),
            layout.pillow(20),
            donor.recommendedFunds({funds: popularCharities})
        ]),
        layout.pillow(20),
        frame.footer([
            core.hyperlink({url: 'Registration', text: 'Charity Registration'})
        ])
    ]);

    yoink.define( frame.frame({contents: body, auth: auth}) );

}

yoink.require(deps, onReady);

