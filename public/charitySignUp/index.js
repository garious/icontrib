var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    'body.html',
    'toa.html'
];


function onReady(E, L, NAV, CORE, html, toaHtml) {

    function inputField(as, xs) {

        var fieldStyle = {
            listStyle: 'none',
            padding: '5px 10px',
            marginBottom: '2px'
        };

        var labelStyle = {
            width: '150px',
            'float': 'left',
            textAlign: 'right',
            marginRight: '5px',
            fontSize: '90%'
        };

        var inputStyle = {
            borderRadius: '2px',
            WebkitBorderRadius: '2px',
            MozBorderRadius: '2px'
        };
                    
        return E.div({style: fieldStyle}, [
            E.label({'for': as.name, style: labelStyle}, as.label),
            E.input({
                'type': as.type, 
                name: as.name, 
                autofocus: 'autofocus', 
                required: as.required, 
                style: inputStyle, 
                placeholder: as.placeholder || ''
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

        return E.legend({style: style}, xs);
    }

    function fieldset(xs) {
        return E.fieldset({style: {border: 'none', marginBottom: '10px'}}, xs);
    }

    function body() {

        var toaDiv = E.div({style: {margin: '15px', height: '300px', overflow: 'auto'}});
        toaDiv.innerHTML = toaHtml;

        var form = E.form({style: {counterReset: 'fieldsets', width: '800px'}}, [
                fieldset([
                    legend('Organization Information'),
                    inputField({label: 'EIN', type: 'number', name: 'ein', required: 'required'}),
                    inputField({label: 'Organization Name', type: 'text', name: 'name'}),
                    inputField({label: 'Company Website', type: 'url', name: 'url', placeholder: 'http://'})
                ]),
                fieldset([
                    legend('Point of Contact'),
                    inputField({label: 'First Name', type: 'text', name: 'firstName', required: 'required'}),
                    inputField({label: 'Last Name', type: 'text', name: 'lastName', required: 'required'}),
                    inputField({label: 'Phone Number', type: 'text', name: 'phoneNumber', placeholder: '(xxx) xxx-xxxx'}),
                    inputField({label: 'Email', type: 'email', name: 'email', placeholder: 'abc@charity.org'})
                ]),
                fieldset([
                    legend(['Terms of Agreement']),
                    toaDiv
                ]),
                CORE.button('Register')

        ]);

        return L.hug([
            L.pillow(250),
            L.spoon([
                CORE.box([
                    E.div({style: {width: '800px'}}, [
                        E.text('Register your organization to recieve recurring contributions from IContrib.org donors.')
                    ])
                ]),
                L.pillow(30),
                CORE.box([form])
            ])
        ]);
    }

    return {
        title: "IContrib - Improve the world today.",
        body: body,
        main: NAV.frame([
            body()
        ])
    };
}

define(deps, onReady);

