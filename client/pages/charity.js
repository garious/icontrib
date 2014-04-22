var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js',
    '/yoink/layout.js',
    '/skin/core.js',
    '/skin/frame.js',
    '/static/charity/' + yoink.params.id + '.json'
];

function onReady(auth, tag, layout, core, frame, charityData) {
    
    function charity(as) {
        as = as || {};
        var user = as.user;
	var box = core.box({
            contents: layout.vcat([
	        core.h2(user.organizationName),
                layout.gap(20),
                layout.hcat([
                    core.image({width: 175, height: 175, borderRadius: 5, url: user.imageUrl, text: user.organizationName}),
                    layout.gap(30),
                    layout.vcat([
                        tag.tag({name: 'p', style: {font: core.defaultFont, width: '600px'}, contents: user.mission}), 
                        layout.gap(20),
                        core.button({href: '/me?donateTo=' + user.cid, text: 'Donate!', loud: true})
                    ])
                ])
            ])
        });

        return layout.vcat([
           box,
           layout.gap(30)
        ]);
    }

    var main = frame.frame({
        contents: charity({user: charityData}),
        auth: auth
    });

    yoink.define(main);

}

yoink.require(deps, onReady);

