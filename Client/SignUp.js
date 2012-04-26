var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js',
    '/Skin/Colors.js'
];

function onReady(Tag, Layout, Core, Frame, Colors) {

    var logo = Tag.tag('a', {href: '/', tabindex: -1, style: {width: '129px', height: '70px'}}, [
        Core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
    ]);

    var badLogin = Tag.tag('span', {hidden: true, style: {height: '20px', width: '200px', color: Colors.red}}, 'bad username or password');


    var formValues = {password: ''};

    function onEmailChanged(evt) {
        formValues.email = evt.target.value;
    }
    function onPasswordChanged(evt) {
        formValues.password = evt.target.value;
    }
    function onConfirmPasswordChanged(evt) {
        formValues.confirmPassword = evt.target.value;
    }

    var email = Core.input({type: 'text', size: 18, width: 300, placeholder: 'Email', autofocus: true, onChange: onEmailChanged});
    var password = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Password', onChange: onPasswordChanged});

    var confirmPassword = Core.input({type: 'password', size: 18, width: 300, placeholder: 'Confirm Password', onChange: onConfirmPasswordChanged});

    function submit(evt) {
        evt.preventDefault();
     
        if (formValues.password !== '' && (formValues.password !== formValues.confirmPassword)) {
            alert('Passwords do not match!');
        } else {
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

    var node = Tag.tag('div', {style: {margin: '0px auto', width: '335px', textAlign: 'center'}}, [
        Tag.tag('br'),
        logo,
        Tag.tag('br'),
        Tag.tag('br'),
        box
    ]);

    define( Frame.webpage(node) );
}
 
require(deps, onReady);

