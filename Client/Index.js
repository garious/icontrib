var deps = [
    '/donor/checkUser.json',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js',
    '/stats/community.json'
];

function onReady(Auth, Tag, Layout, Frame, Core, PopularCharities, Donor, Community) {

    var body = Layout.spoon([
        Layout.hug([
            Core.box({
                width: 600,
                contents: Donor.profile1({user: Community})
            }),
            Layout.pillow(20),
            Donor.recommendedFunds({funds: PopularCharities})
        ]),
        Layout.pillow(20),
        Frame.footer([
            Core.hyperlink({url: 'Registration', text: 'Charity Registration'})
        ])
    ]);

    Yoink.define( Frame.frame({contents: body, auth: Auth}) );

}

Yoink.require(deps, onReady);

