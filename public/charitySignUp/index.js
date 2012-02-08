var deps = [
    '/tag/tag.js', 
    '/tag/layout.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    'toa.html',
    '/jquery/jquery-mod.js',
    '/jsonform/jsonform.js'
];


function onReady(E, L, NAV, CORE, toaHtml, $, JF) {
    function inputField(input, as, xs) {

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
        input['type'] = as.type;
        input.name = as.name;
        input.autofocus = 'autofocus';
        input.syte = inputStyle;
        input.placeholder = as.placeholder || '';

        return E.div({style: fieldStyle}, [
            E.label({'for': as.name, style: labelStyle}, as.label), 
            input
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

        var toaDiv = E.div({style: {margin: '15px', height: '220px', overflow: 'auto'}});
        toaDiv.innerHTML = toaHtml;
        var info = { ein: null,
                     organizationName: null,
                     companyWebsite: null
                   };
        var poc = { firstName: null,
                    lastName: null,
                    phone: null,
                    email: null
                  };
        //stupid schema, the 'null' services as a sentinal when i traverse it
        var schema = { info: info, poc: poc };
        //now i have an object with a bunch of empty inputs, whose layout matches my schema
        //i can traverse the schema in parallel with the object and reference the input fields
        var inputs = JF.map(schema, schema, {}, JF.toInput);
        var oi = inputs.info; 
        var pc = inputs.poc;
        var name        = inputField(oi.organizationName, {label: 'Organization Name', type: 'text', name: 'name'});
        var ein         = inputField(oi.ein,              {label: 'EIN', type: 'text', name: 'ein', required: 'required'});
        var url         = inputField(oi.companyWebsite,   {label: 'Company Website', type: 'url', name: 'url', placeholder: 'http://'});
        var firstName   = inputField(pc.firstName,        {label: 'First Name', type: 'text', name: 'firstName', required: 'required'});
        var lastName    = inputField(pc.lastName,         {label: 'Last Name', type: 'text', name: 'lastName', required: 'required'});
        var phoneNumber = inputField(pc.phone,            {label: 'Phone Number', type: 'text', name: 'phoneNumber', placeholder: '(xxx) xxx-xxxx'});
        var email       = inputField(pc.email,            {label: 'Email', type: 'email', name: 'email', placeholder: 'abc@charity.org'});
        //var checkbox    = E.input({type: 'checkbox', style: {width: '200px'}}, 'I agree to the terms above');
        var register    = CORE.button('Register!');
        var form = E.form({style: {counterReset: 'fieldsets', width: '800px'}}, [
                fieldset([legend('Organization Information'), ein, name, url ]),
                fieldset([legend('Point of Contact'), firstName, lastName, phoneNumber, email ]),
                fieldset([legend('Interchange Fee'), E.div({style: {left: '30px', position: 'absolute'}}, [CORE.h4('3.9%')])]),
                fieldset([legend(['Terms of Agreement']), toaDiv/*, checkbox*/ ]),
                register
        ]);
        var swapNode = function(newNode, oldNode) {
            oldNode.parentNode.replaceChild(newNode, oldNode);
            return newNode;
        };
        //fetch the current charity info
        $.ajax({
            type: "GET",
            url: '/charity/get',
            dataType: "json",
            success: function(data) {
 console.log("Anatoly sucks:", data);
                if(data.Right) {
                    var fromVal = function(name, value, rv) { 
                        rv.value = value; 
                        return rv; 
                    };
                    inputs = JF.map(schema, data.Right, inputs, fromVal);
                    register = swapNode(E.input({type: 'submit', value : 'Update' }), register);
                }
            }
        });

        $(form).click(function (e) {
            e.preventDefault();
            var values = JF.map(schema, inputs, {}, JF.toVal);
            var dataString = JSON.stringify(values);
            $.ajax({
                type: "POST",
                url: '/charity/update',
                data: dataString,
                dataType: "json",
                success: function(data) {
                    var dataString = JSON.stringify(data);
                    console.log(dataString);
                }
             });
        });
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

    define({
        title: 'IContrib.org',
        body: body,
        main: NAV.frame([
            body()
        ])
    });
}

require(deps, onReady);

