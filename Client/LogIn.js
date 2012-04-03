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

    var logo = Tag.a({href: '/', tabindex: -1, style: {width: '129px', height: '70px'}}, [
        Core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
    ]);

    //var hidden = Observable.observe('hidden');

    var badLogin = Tag.span({style: {visibility: 'hidden', color: Colors.red}}, 'bad username or password');

    var formValues = {};

    function onEmailChanged(evt) {
        formValues.email = evt.target.value;
    }
    function onPasswordChanged(evt) {
        formValues.password = evt.target.value;
    }

    var email = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Email', onChange: onEmailChanged});
    var password = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Password', onChange: onPasswordChanged});

    function submit(evt) {
        evt.preventDefault();
        Frame.post('/auth/login', formValues, function(dat) {
            var data = JSON.parse(dat);
            if(data.Left) {
                'noop';
                //hidden.set('visible');
            } else {
                window.location = '/Me';
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
        Layout.pillow(0, 15),
        Core.button({text: 'Log in', onClick: submit, width: 314})
    ]);

    var box = Core.box({
        contents: widget,
        onKeyUp: onKeyUp
    });

    var node = Tag.div({style: {margin: 'auto', width: '355px', textAlign: 'center'}}, [
        Tag.br(),
        logo,
        Tag.br(),
        Tag.br(),
        box
    ]);

    define( Frame.webpage(node) );
}
 
require(deps, onReady);

