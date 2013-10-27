var deps = [
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/yoink/observable.js', 
    '/skin/core.js',
    '/skin/frame.js',
    '/skin/colors.js'
];

function onReady(tag, layout, observable, core, frame, colors) {

    var logo = tag.tag({
        name: 'a',
        attributes: {href: '/', tabindex: -1},
        style: {width: '129px', height: '70px'},
        contents: [
            core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
        ]
    });

    var hidden = Observable.observe('hidden');

    var badLogin = tag.tag({
        name: 'span',
        style: {visibility: hidden, color: colors.red},
        contents: 'bad username or password'
    });

    var formValues = {};

    function onEmailChanged(evt) {
        formValues.email = evt.target.value;
    }
    function onPasswordChanged(evt) {
        formValues.password = evt.target.value;
    }

    var email = core.input({type: 'text', size: 18, width: 300, placeholder: 'Email', autofocus: true, onChange: onEmailChanged});
    var password = core.input({type: 'password', size: 18, width: 300, placeholder: 'Password', onChange: onPasswordChanged});

    function submit(evt) {
        evt.preventDefault();
        frame.post('/auth/login', formValues, function(dat) {
            var data = JSON.parse(dat);
            if(data.Left) {
                hidden.set('visible');
            } else {
                // Cheap trick to go back a page and ensure it refreshes.
                var sep = document.referrer.indexOf('?') === -1 ? '?' : '&';
                location.href = document.referrer + sep + 'date=' + new Date().valueOf();
            }
        });
    }

    function onKeyUp (evt) {
        if (evt.keyCode === 13) {
           submit(evt);
        }
    }

    var widget = layout.spoon([
        email,
        layout.pillow(0, 15),
        password,
        badLogin,
        core.button({text: 'Log in', onClick: submit, width: 314})
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

