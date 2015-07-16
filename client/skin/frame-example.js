var frame = require('./frame');
var core = require('./core');

module.exports = frame.footer({style: {backgroundColor: '#EEE'}}, [
    core.hyperlink({url: '#', text: 'blah'})
]);
