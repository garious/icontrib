var deps = [
    '/Tag/Interface.js',
    '/Tag/TwoDimensional.js',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Tag/Observable.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js',
    '/Skin/Colors.js'
];

function onReady(Iface, TwoDim, Tag, Layout, Observable, Core, Frame, Colors) {

    var logo = Tag.tag({
        name: 'a',
        attributes: {href: '/', tabindex: -1},
        style: {width: '129px', height: '70px'},
        contents: [
            Core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
        ]
    });

    var hidden = Observable.observe('hidden');

    var badLogin = Tag.tag({
        name: 'span',
        style: {visibility: hidden, color: Colors.red},
        contents: 'bad username or password'
    });

    var formValues = {};

    function onEmailChanged(evt) {
        formValues.email = evt.target.value;
    }
    function onPasswordChanged(evt) {
        formValues.password = evt.target.value;
    }

    var email = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Email', autofocus: true, onChange: onEmailChanged});
    var password = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Password', onChange: onPasswordChanged});

    function submit(evt) {
        evt.preventDefault();
        Frame.post('/auth/login', formValues, function(dat) {
            var data = JSON.parse(dat);
            if(data.Left) {
                hidden.set('visible');
            } else {
                // Cheap trick to go back a page and ensure it refreshes.
                location.href = document.referrer + '?date=' + new Date().valueOf();
            }
        });
    }

    function onKeyUp (evt) {
        if (evt.keyCode === 13) {
           submit(evt);
        }
    }

    var widget = Layout.spoon([
        email,
        Layout.pillow(0, 15),
        password,
        badLogin,
        Core.button({text: 'Log in', onClick: submit, width: 314})
    ]);

    var box = Core.box({
        contents: widget,
        onKeyUp: onKeyUp
    });

    var node = Tag.tag({
        name: 'div',
        style: {margin: 'auto', width: '355px', textAlign: 'center'},
        contents: [
            Tag.tag('br'),
            logo,
            Tag.tag('br'),
            Tag.tag('br'),
            box
        ]
    });

    Yoink.define( Frame.webpage(node) );
}
 
Yoink.require(deps, onReady);

