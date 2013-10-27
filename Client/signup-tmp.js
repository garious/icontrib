var deps = [
    '/yoink/tag.js', 
    '/yoink/layout.js', 
    '/Skin/Core.js',
    '/Skin/Frame.js',
    '/Skin/Colors.js'
];

function onReady(tag, layout, core, frame, colors) {

    var logo = tag.tag({
        name: 'a',
        attributes: {href: '/', tabindex: -1},
        style: {width: '129px', height: '70px'}, 
        contents: [
            core.image({url: '/Skin/logo.png', text: 'IContrib Home'})
        ]
    });

    var badLogin = tag.tag({
        name: 'span',
        attributes: {hidden: true},
        style: {height: '20px', width: '200px', color: colors.red},
        contents: 'bad username or password'
    });

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

    var email = core.input({type: 'text', size: 18, width: 300, placeholder: 'Email', autofocus: true, onChange: onEmailChanged});
    var password = core.input({type: 'password', size: 18, width: 300, placeholder: 'Password', onChange: onPasswordChanged});

    var confirmPassword = core.input({type: 'password', size: 18, width: 300, placeholder: 'Confirm Password', onChange: onConfirmPasswordChanged});

    function submit(evt) {
        evt.preventDefault();
     
        if (formValues.password !== '' && (formValues.password !== formValues.confirmPassword)) {
            alert('Passwords do not match!');
        } else {
            frame.post('/auth/add', formValues, function(dat) {
                var data = JSON.parse(dat);
                if(data.Left) {
                    badLogin.hidden = false;
                } else {
                    // Cheap trick to go back a page and ensure it refreshes.
                    var sep = document.referrer.indexOf('?') === -1 ? '?' : '&';
                    location.href = document.referrer + sep + 'date=' + new Date().valueOf();
                }
            });
        }
    }

    function onKeyUp (evt) {
        if (evt.keyCode === 13) {
           submit(evt);
        }
    }

    var widget = layout.spoon([
        core.h4('Create an account'),
        layout.pillow(0, 15),
        email,
        layout.pillow(0, 15),
        password,
        layout.pillow(0, 15),
        confirmPassword,
        layout.pillow(0, 15),
        core.button({text: 'Create account', onClick: submit, width: 314})
    ]);

    var box = core.box({
        width: 355,
        contents: widget,
        onKeyUp: onKeyUp
    });

    var node = tag.tag({
        name: 'div',
        style: {margin: '0px auto', width: '335px', textAlign: 'center'},
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

