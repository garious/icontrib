var layout = require('poochie/layout');
var frame = require('../skin/frame');
var core = require('../skin/core');
var donor = require('../skin/donor');
var dydata = require('../skin/dydata');

var auth = dydata.jsonPublisher('/donor/checkUser.json', {});
var popularCharities = dydata.jsonPublisher('/charity/popular.json', []);
var user = dydata.jsonPublisher('/static/donor/' + yoink.params.id + '.json', {});

var box = core.box({
    width: 600,
    contents: donor.profile({user: user})
});

var body = layout.hcat([
    box,
    layout.gap(20),
    donor.recommendedFunds({funds: popularCharities})
]);

module.exports = frame.frame({contents: body, auth: auth});
