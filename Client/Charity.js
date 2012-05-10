var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Core.js', 
    '/Skin/Frame.js',
    '/charity/' + Yoink.params.id + '.json'
];

function onReady(Tag, Layout, Core, Frame, User) {
    
    function charity(as) {
        as = as || {};
        var user = as.user;
	var box = Core.box({
            contents: Layout.spoon([
	        Core.h2(user.organizationName),
                Layout.pillow(20),
                Layout.hug([
                    Core.image({width: 175, height: 175, borderRadius: 5, url: user.imageUrl, text: user.organizationName}),
                    Layout.pillow(30),
                    Layout.spoon([
                        Tag.tag({name: 'p', style: {font: Core.defaultFont, width: '600px'}, contents: user.mission}), 
                        Layout.pillow(20),
                        Core.button({href: '/Me?donateTo=' + user.cid, text: 'Donate!', loud: true})
                    ])
                ])
            ])
        });

        return Layout.spoon([
           box,
           Layout.pillow(30)
        ]);
    }

    var main = Frame.frame(charity({user: User}));

    Yoink.define(main);

}

Yoink.require(deps, onReady);

