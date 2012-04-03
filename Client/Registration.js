var deps = [
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Skin/Frame.js', 
    '/Skin/Core.js', 
    'toa.html',
    '/charity/get.json'
];

function onReady(Tag, Layout, Frame, Core, toaHtml, Charity) {

    function inputField(as) {

        var fieldStyle = {
            listStyle: 'none',
            padding: '5px 10px',
            marginBottom: '2px',
            height: '20px'  // TODO: derive this from field contents
        };

        var labelStyle = {
            width: '150px',
            'float': 'left',
            textAlign: 'right',
            marginRight: '5px',
            fontSize: '90%',
            font: Core.defaultFont
        };

        var inputStyle = {
            borderRadius: '2px',
            WebkitBorderRadius: '2px',
            MozBorderRadius: '2px'
        };

        return Tag.div({style: fieldStyle}, [
            Tag.label({'for': as.name, style: labelStyle}, as.label), 
            Tag.input({
                type: as.type,
                name: as.name,
                autofocus: 'autofocus',
                style: inputStyle,
                placeholder: as.placeholder || ''
            }, null, {
                change: function(evt) {
                    as.values[as.name] = evt.target.value;
                }
            })
        ]);
    }

    function legend(xs) {
        var style = {
            color: '#000046',
            fontSize: '16px',
            fontWeight: 'bold',
            paddingTop: '10px',
            textShadow: '0 1px 1px #141451' 
        };

        return Tag.legend({style: style}, xs);
    }

    function fieldset(xs) {
        return Tag.fieldset({style: {border: 'none', marginBottom: '10px', height: (xs * 20) + 'px'}}, xs);
    }

    function body() {

        var toaDiv = Tag.div({style: {margin: '15px', height: '230px', overflow: 'auto', font: Core.defaultFont}});
        toaDiv.innerHTML = toaHtml;

        var values = {
             ein: '',
             organizationName: '',
             companyWebsite: '',
             paymentAddress: ''
        };

        var name        = inputField({label: 'Organization Name', type: 'text', name: 'organizationName', values: values});
        var ein         = inputField({label: 'EIN', type: 'text', name: 'ein', required: 'required', values: values});
        var url         = inputField({label: 'Website URL', type: 'url', name: 'companyWebsite', placeholder: 'http://', values: values});
        var payAddr     = inputField({label: 'PayPal address', type: 'email', name: 'paymentAddress', placeholder: 'donations@charity.org', values: values});

        function onRegister (evt) {
            evt.preventDefault();
            var dataString = JSON.stringify(values);
            Frame.post('/charity/update', dataString, function(data) {
                var dataString = JSON.stringify(data);
                console.log(dataString);
            });
        }

        var register = Core.button({text: 'Register!', loud: true, onClick: onRegister});

        var form = Tag.form({style: {counterReset: 'fieldsets', width: '800px', height: '720px'}}, [
                fieldset([legend('Organization Information'), ein, name, url, payAddr ]),
                //fieldset([legend('Point of Contact'), firstName, lastName, phoneNumber, email ]),
                fieldset([legend('Interchange Fee'), Tag.div({style: {left: '30px'}}, [Core.h4('3.9%')])]),
                fieldset([legend(['Terms of Agreement']), toaDiv/*, checkbox*/ ]),
                register
        ]);

        return Layout.hug([
            Layout.spoon([
                Core.box({
                    width: 800,
                    contents: Core.p('Register your organization to recieve recurring contributions from IContrib.org donors.')
                }),
                Layout.pillow(30),
                Core.box({contents: form}),
                Layout.pillow(30)
            ])
        ]);
    }

    define( Frame.frame(body()) );
}

require(deps, onReady);

