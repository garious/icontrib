var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/charity/popular.json', 
    '/ui/donor.js'
];

function onInitialReady(MostInfluentialId) {

    function onReady(Tag, Layout, Nav, Core, PopularCharities, Donor, MostInfluential) {

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
            Nav.footer([
                Core.hyperlink({url: 'Registration', text: 'Charity Registration'})
            ])
        ]);

        define( Nav.frame(body) );

    }

    deps.push('/donor/' + MostInfluentialId + '.json');
    require(deps, onReady);
}

require(initialDeps, onInitialReady);

