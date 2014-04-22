var deps = [
    '/donor/checkUser.json',
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/yoink/observable.js', 
    '/skin/core.js',
    '/skin/frame.js',
    '/skin/colors.js'
];

function onReady(auth, tag, layout, observable, core, frame, colors) {
    if (auth.Left) {
        window.location = auth.Left.loginUrl;
        return;
    }

    var logo = tag.tag({
        name: 'a',
        attributes: {href: '/', tabindex: -1},
        style: {width: '129px', height: '70px'},
        contents: [
            core.image({url: '/skin/logo.png', text: 'IContrib Home'})
        ]
    });

    var hidden = observable.observe('hidden');

    var badLogin = tag.tag({
        name: 'span',
        style: {visibility: hidden, color: colors.red},
        contents: 'bad username or password'
    });

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

    var orgName = core.input({type: 'text', size: 18, width: 300, placeholder: 'Organization name', autofocus: true, onChange: mkOnChanged('organizationName')});
    var orgEin = core.input({type: 'text', size: 18, width: 300, placeholder: 'Employer Identification Number (EIN)', onChange: mkOnChanged('ein')});
    var orgUrl = core.input({type: 'text', size: 18, width: 300, placeholder: 'Website URL', onChange: mkOnChanged('companyWebsite')});
    var orgPayPal = core.input({type: 'text', size: 18, width: 300, placeholder: 'PayPal address', onChange: mkOnChanged('paymentAddress')});

    function onRegister (evt) {
        evt.preventDefault();
        frame.post('/charity/update', formValues, function(data) {
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

    var widget = layout.spoon([
        core.h4('Charity Registration'),
        layout.pillow(0, 15),
        orgName,
        layout.pillow(0, 15),
        orgEin,
        layout.pillow(0, 15),
        orgUrl,
        layout.pillow(0, 15),
        orgPayPal,
        layout.pillow(0, 10),
        core.button({text: 'Register organization', onClick: onRegister, width: 314})
    ]);

    var box = core.box({
        contents: widget,
        onKeyUp: onKeyUp
    });

    var node = tag.tag({
        name: 'div',
        style: {margin: 'auto', width: '355px', textAlign: 'center'},
        contents: [
            tag.tag('br'),
            logo,
            tag.tag('br'),
            tag.tag('br'),
            box
        ]
    });

    yoink.define( frame.webpage(node) );
}
 
yoink.require(deps, onReady);

