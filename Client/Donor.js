var deps = [
    '/donor/checkUser.json',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js',
    '/donor/' + Yoink.params.id + '.json'
];

function onReady(Auth, Tag, Layout, Frame, Core, PopularCharities, Donor, User) {

    var box = Core.box({
        width: 600,
        contents: Donor.profile({user: User})
    });

    var body = Layout.hug([
        box,
        Layout.pillow(20),
        Donor.recommendedFunds({funds: PopularCharities})
    ]);

    Yoink.define( Frame.frame({contents: body, auth: Auth}) );

}

Yoink.require(deps, onReady);

