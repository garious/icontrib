var dom = require('poochie/dom');
var layout = require('poochie/layout');
var core = require('../skin/core');
var frame = require('../skin/frame');
var dydata = require('../skin/dydata');

var auth = dydata.jsonPublisher('/donor/checkUser.json', {});
var charityData = dydata.jsonPublisher('/static/charity' + yoink.params.id + '.json', {});

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
                    dom.element({name: 'p', style: {font: core.defaultFont, width: '600px'}, contents: user.mission}),
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

module.exports = frame.frame({
    contents: charity({user: charityData}),
    auth: auth
});
