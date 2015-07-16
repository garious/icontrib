var layout = require('poochie/layout');
var frame = require('../skin/frame');
var core = require('../skin/core');
var donor = require('../skin/donor');
var dydata = require('../skin/dydata');

var auth = dydata.jsonPublisher('/donor/checkUser.json', {});
var popularCharities = dydata.jsonPublisher('/charity/popular.json', []);
var community = dydata.jsonPublisher('/stats/community.json', {});

var body = layout.vcat([
    layout.hcat([
        core.box({
            width: 600,
            contents: donor.profile({user: community})
        }),
        layout.gap(20),
        donor.recommendedFunds({funds: popularCharities})
    ]),
    layout.gap(20),
    frame.footer([
        core.hyperlink({url: 'registration', text: 'Charity Registration'})
    ])
]);

module.exports = frame.frame({contents: body, auth: auth});
