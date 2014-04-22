var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/skin/frame.js',
    '/skin/core.js',
    '/charity/popular.json', 
    '/skin/donor.js',
    '/stats/community.json'
];

function onReady(auth, tag, layout, frame, core, popularCharities, donor, community) {

    var body = layout.vcat([
        layout.hcat([
            core.box({
                width: 600,
                contents: donor.profile({user: community})
            }),
            layout.gap(20),
            donor.recommendedFunds({funds: popularCharities})
        ]),
        layout.gap(20),
        frame.footer([
            core.hyperlink({url: 'registration', text: 'Charity Registration'})
        ])
    ]);

    yoink.define( frame.frame({contents: body, auth: auth}) );

}

yoink.require(deps, onReady);

