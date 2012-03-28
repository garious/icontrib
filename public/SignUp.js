var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js'
];

function onReady(Tag, Layout, Core, Frame) {

    var logo = Tag.a({href: '/', style: {width: '129px', height: '70px'}}, [
        Core.image({url: '/ui/logo.png', text: 'IContrib Home'})
    ]);

    var badLogin = Tag.span({hidden: true, style: {height: '20px', width: '200px', color: 'red'}}, 'bad username or password');

    var email = Core.input({type: 'text', size: 18, width: 300});
    var password = Core.input({type: 'password', size: 18, width: 300});

    function submit(evt) {
        evt.preventDefault();
        var formValues = {
            email: email.value,
            password: password.value 
        };
        Frame.post('/auth/login', formValues, function(dat) {
            var data = JSON.parse(dat);
            if(data.Left) {
                badLogin.hidden = false;
            } else {
                window.location = '/Me/';
            }
        });
    }

    var widget = Layout.spoon([
        email,
        Layout.pillow(0, 30),
        password,
        Layout.pillow(0, 30),
        Core.button({text: 'Log in', onClick: submit})
    ]);

    widget.addEventListener('keyup', function(evt) {
        if (evt.keyCode === 13) {
           submit(evt);
        }
    });

    var box = Core.box({
        width: 340,
        contents: widget
    });

    var main = Layout.spoon([
       Layout.pillow(0, 20),
       logo,
       Layout.pillow(0, 30),
       box 
    ]);

    var node = Tag.div({style: {margin: '0px auto', height: '100%', width: '460px'}}, [
        main
    ]);

    define( Frame.webpage(node) );
}
 
require(deps, onReady);

