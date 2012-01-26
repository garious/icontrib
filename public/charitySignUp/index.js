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

        var toaDiv = E.div({style: {margin: '15px', height: '300px', overflow: 'auto'}});
        toaDiv.innerHTML = toaHtml;
        var info = { OrganizationInfo: { 
                        ein: null,
                        organizationName: null,
                        companyWebsite: null
                   }};
        var poc = { PointOfContact: { 
                        firstName: null,
                        lastName: null,
                        phone: null,
                        email: null
                  }};
        //stupid schema, the 'null' services as a sentinal when i traverse it
        var schema = {  CharityInfo: { info: info, poc: poc } };
        //now i have an object with a bunch of empty inputs, whose layout matches my schema
        //i can traverse the schema in parallel with the object and reference the input fields
        var inputs = JF.map(schema, schema, {}, JF.toInput);
        var oi = inputs.CharityInfo.info.OrganizationInfo; 
        var pc = inputs.CharityInfo.poc.PointOfContact;
        var name        = inputField(oi.organizationName, {label: 'Organization Name', type: 'text', name: 'name'});
        var ein         = inputField(oi.ein,              {label: 'EIN', type: 'number', name: 'ein', required: 'required'});
        var url         = inputField(oi.companyWebsite,   {label: 'Company Website', type: 'url', name: 'url', placeholder: 'http://'});
        var firstName   = inputField(pc.firstName,        {label: 'First Name', type: 'text', name: 'firstName', required: 'required'});
        var lastName    = inputField(pc.lastName,         {label: 'Last Name', type: 'text', name: 'lastName', required: 'required'});
        var phoneNumber = inputField(pc.phone,            {label: 'Phone Number', type: 'text', name: 'phoneNumber', placeholder: '(xxx) xxx-xxxx'});
        var email       = inputField(pc.email,            {label: 'Email', type: 'email', name: 'email', placeholder: 'abc@charity.org'});
        var register    = E.input({type: 'submit', value : 'Register' });
        var form = E.form({style: {counterReset: 'fieldsets', width: '800px'}}, [
                fieldset([legend('Organization Information'), ein, name, url ]),
                fieldset([legend('Point of Contact'), firstName, lastName, phoneNumber, email ]),
                fieldset([legend(['Terms of Agreement']), toaDiv ]),
                register
        ]);
        var swapNode = function(newNode, oldNode) {
            oldNode.parentNode.replaceChild(newNode, oldNode);
            return newNode;
        };
        //fetch the current charity info
        $.ajax({
            type: "GET",
            url: '/charity/getInfo',
            dataType: "json",
            success: function(data) {
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

        $(form).submit(function (e) {
            e.preventDefault();
            var values = JF.map(schema, inputs, {}, JF.toVal);
            var dataString = JSON.stringify(values);
            $.ajax({
                type: "POST",
                url: '/charity/updateInfo',
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

