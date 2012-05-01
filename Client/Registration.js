var deps = [
    '/auth/check.json',
    '/Tag/Interface.js',
    '/Tag/TwoDimensional.js',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Tag/Observable.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js',
    '/Skin/Colors.js'
];

function onReady(Auth, Iface, TwoDim, Tag, Layout, Observable, Core, Frame, Colors) {
    if (Auth.Left) {
        window.location = '/SignUp';
        return;
    }

    var logo = Tag.tag('a', {href: '/', tabindex: -1, style: {width: '129px', height: '70px'}}, [
        Core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
    ]);

    var hidden = Observable.observe('hidden');

    var badLogin = Tag.tag('span', {style: {visibility: hidden, color: Colors.red}}, 'bad username or password');

    var formValues = {
         ein: '',
         organizationName: '',
         companyWebsite: '',
         paymentAddress: ''
    };

    function mkOnChanged(nm) {
        return function (evt) {
            formValues[nm] = evt.target.value;
        };
    }

    var orgName = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Organization name', autofocus: true, onChange: mkOnChanged('organizationName')});
    var orgEin = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Employer Identification Number (EIN)', onChange: mkOnChanged('ein')});
    var orgUrl = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Website URL', onChange: mkOnChanged('companyWebsite')});
    var orgPayPal = Core.input({type: 'text', size: 18, width: 300, placeholder: 'PayPal address', onChange: mkOnChanged('paymentAddress')});

    function onRegister (evt) {
        evt.preventDefault();
        Frame.post('/charity/update', formValues, function(data) {
            var dataString = JSON.stringify(data);
            console.log(dataString);
        });
    }

    function onKeyUp (evt) {
        evt.preventDefault();
        if (evt.keyCode === 13) {
           onRegister(evt);
        }
    }

    var widget = Layout.spoon([
        Core.h4('Charity Registration'),
        Layout.pillow(0, 15),
        orgName,
        Layout.pillow(0, 15),
        orgEin,
        Layout.pillow(0, 15),
        orgUrl,
        Layout.pillow(0, 15),
        orgPayPal,
        Layout.pillow(0, 10),
        Layout.hug([
            Tag.tag('input', {type: 'checkbox', style: {marginTop: '5px'}}),
            Layout.pillow(5, 0),
            Core.p('I agree to the'),
            Layout.pillow(5, 0),
            Core.hyperlink({text: 'Terms and Conditions', url: 'toa.html'})
        ]),
        Layout.pillow(0, 10),
        Core.button({text: 'Register organization', onClick: onRegister, width: 314})
    ]);

    var box = Core.box({
        contents: widget,
        onKeyUp: onKeyUp
    });

    var node = Tag.tag('div', {style: {margin: 'auto', width: '355px', textAlign: 'center'}}, [
        Tag.tag('br'),
        logo,
        Tag.tag('br'),
        Tag.tag('br'),
        box
    ]);

    Yoink.define( Frame.webpage(node) );
}
 
Yoink.require(deps, onReady);

