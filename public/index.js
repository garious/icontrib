var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/colors.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/charity/popular.json', 
    '/ui/donor.js'
];

function onInitialReady(MostInfluentialId) {

    function onReady(Tag, Layout, Colors, Nav, Core, PopularCharities, Donor, MostInfluential) {

        var sep = Layout.pillow(20);

        var listItems = [
            Core.h5({
                color: Colors.greenText,
                text: 'Recommended Funds'
            })
        ];

        var pad = Layout.pillow(0, 10);

        for (var i = 0; i < PopularCharities.length; i += 1) {
            var x = PopularCharities[i];
            listItems.push( pad );
            listItems.push( Core.hr({width: 300}) );
            listItems.push( pad );

            var e = Layout.hug([
                Tag.img({src: x.imageUrl, style: {width: '50px', height: '50px'}}),  // TODO: use the fund's actual image
                Layout.pillow(20, 0),
                Core.a({href: '/charity/?id=' + x.cid}, x.name)
            ]);

            listItems.push(e);
        }

        var body = Layout.spoon([
            Layout.hug([
                Core.box({width: 600}, [
                    Donor.profile({user: MostInfluential})
                ]),
                sep,
                Core.box({width: 340}, [
                    Layout.spoon(listItems)
                ])
            ]),
            sep,
            Nav.footer([
                Core.a({href: 'charitySignUp/'}, 'Charity Registration')
            ])
        ]);

        define( Nav.frame([body]) );

    }

    deps.push('/donor/' + MostInfluentialId + '.json');
    require(deps, onReady);
}

require(initialDeps, onInitialReady);

