var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    '/charity/popular.json', 
    '/Skin/Donor.js'
];

function onInitialReady(MostInfluentialId) {

    function onReady(Tag, Layout, Frame, Core, PopularCharities, Donor, MostInfluential) {

        var mostInfluential = Core.box({
            width: 600,
            contents: Donor.profile({user: MostInfluential})
        });

        var sep = Layout.pillow(20);

        var body = Layout.spoon([
            Layout.hug([
                mostInfluential,
                sep,
                Donor.recommendedFunds({funds: PopularCharities})
            ]),
            sep,
            Frame.footer([
                Core.hyperlink({url: 'Registration', text: 'Charity Registration'})
            ])
        ]);

        define( Frame.frame(body) );

    }

    deps.push('/donor/' + MostInfluentialId + '.json');
    require(deps, onReady);
}

require(initialDeps, onInitialReady);

