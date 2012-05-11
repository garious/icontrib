var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/donor/checkUser.json',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js'
];

function onInitialReady(MostInfluentialId) {

    function onReady(Auth, Tag, Layout, Frame, Core, PopularCharities, Donor, MostInfluential) {

        var mostInfluential = Core.box({
            width: 600,
            contents: Donor.profile1({user: MostInfluential})
        });

        var body = Layout.spoon([
            Layout.hug([
                mostInfluential,
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

    deps.push('/donor/' + MostInfluentialId + '.json');
    Yoink.require(deps, onReady);
}

Yoink.require(initialDeps, onInitialReady);

