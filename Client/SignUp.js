var deps = [
    '/Tag/Interface.js',
    '/Tag/TwoDimensional.js',
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js',
    '/Skin/Colors.js'
];

function onReady(Iface, TwoDim, Tag, Layout, Core, Frame, Colors) {

    var logo = Tag.a({href: '/', tabindex: -1, style: {width: '129px', height: '70px'}}, [
        Core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
    ]);

    var badLogin = Tag.span({hidden: true, style: {height: '20px', width: '200px', color: Colors.red}}, 'bad username or password');

    var email = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Email'});
    var password = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Password'});
    var confirmPassword = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Confirm Password'});

    function submit(evt) {
        evt.preventDefault();
     
        if (password.value !== '' || (password.value !== confirmPassword.value)) {
            alert('Passwords do not match!');
        } else {
            var formValues = {
                email: email.value,
                password: password.value
            };
            Frame.post('/auth/add', formValues, function(dat) {
                var data = JSON.parse(dat);
                if(data.Left) {
                    badLogin.hidden = false;
                } else {
                    window.location = '/Me';
                }
            });
        }
    }

    function onKeyUp (evt) {
        if (evt.keyCode === 13) {
           submit(evt);
        }
    }

    var widget = Layout.spoon([
        Core.h4('Create an Account'),
        Layout.pillow(0, 15),
        email,
        Layout.pillow(0, 15),
        password,
        Layout.pillow(0, 15),
        confirmPassword,
        Layout.pillow(0, 15),
        Core.button({text: 'Create Account', onClick: submit, width: 314})
    ]);

    var box = Core.box({
        width: 355,
        contents: widget,
        onKeyUp: onKeyUp
    });

    var iface = Iface.getInterface(box, TwoDim.twoDimensionalId);
    var dim = iface.getDimensions(box);

    var node = Tag.div({style: {margin: '0px auto', height: '100%', width: dim.width + 'px', textAlign: 'center'}}, [
        Tag.br(),
        logo,
        Tag.br(),
        Tag.br(),
        box
    ]);

    define( Frame.webpage(node) );
}
 
require(deps, onReady);

