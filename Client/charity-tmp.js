var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js',
    '/yoink/layout.js',
    '/Skin/Core.js', 
    '/Skin/Frame.js',
    '/charity/' + yoink.params.id + '.json'
];

function onReady(auth, tag, layout, core, frame, charityData) {
    
    function charity(as) {
        as = as || {};
        var user = as.user;
	var box = core.box({
            contents: layout.spoon([
	        core.h2(user.organizationName),
                layout.pillow(20),
                layout.hug([
                    core.image({width: 175, height: 175, borderRadius: 5, url: user.imageUrl, text: user.organizationName}),
                    layout.pillow(30),
                    layout.spoon([
                        tag.tag({name: 'p', style: {font: core.defaultFont, width: '600px'}, contents: user.mission}), 
                        layout.pillow(20),
                        core.button({href: '/Me?donateTo=' + user.cid, text: 'Donate!', loud: true})
                    ])
                ])
            ])
        });

        return layout.spoon([
           box,
           layout.pillow(30)
        ]);
    }

    var main = frame.frame({
        contents: charity({user: charityData}),
        auth: auth
    });

    yoink.define(main);

}

yoink.require(deps, onReady);

