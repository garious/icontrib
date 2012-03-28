var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    'toa.html',
    '/js/jsonform.js',
    '/charity/get.json'
];

// TODO: Move this to a shared location
function post(path, params, callback) {
    var req = new XMLHttpRequest();
    req.onreadystatechange = function () {
        if (req.readyState === 4) {
            callback(req.responseText);
        }
    };

    var body = JSON.stringify(params);

    req.open('POST', path, true);
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

    req.send(body);
}


function onReady(Tag, Layout, Nav, Core, toaHtml, JsonForm, Charity) {

    function inputField(input, as, xs) {

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
        input['type'] = as.type;
        input.name = as.name;
        input.autofocus = 'autofocus';
        input.syte = inputStyle;
        input.placeholder = as.placeholder || '';

        return Tag.div({style: fieldStyle}, [
            Tag.label({'for': as.name, style: labelStyle}, as.label), 
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

        return Tag.legend({style: style}, xs);
    }

    function fieldset(xs) {
        return Tag.fieldset({style: {border: 'none', marginBottom: '10px', height: (xs * 20) + 'px'}}, xs);
    }

    function body() {

        var toaDiv = Tag.div({style: {margin: '15px', height: '230px', overflow: 'auto', font: Core.defaultFont}});
        toaDiv.innerHTML = toaHtml;
        var info = { ein: null,
                     organizationName: null,
                     companyWebsite: null,
                     paymentAddress: null
                   };
        //var poc = { firstName: null,
        //            lastName: null,
        //            phone: null,
        //            email: null
        //          };
        //stupid schema, the 'null' services as a sentinal when i traverse it
        //var schema = { info: info, poc: poc };
        var schema = info;
        //now i have an object with a bunch of empty inputs, whose layout matches my schema
        //i can traverse the schema in parallel with the object and reference the input fields
        var inputs = JsonForm.map(schema, schema, {}, JsonForm.toInput);
        //var pc = inputs.poc;
        var name        = inputField(inputs.organizationName, {label: 'Organization Name', type: 'text', name: 'name'});
        var ein         = inputField(inputs.ein,              {label: 'EIN', type: 'text', name: 'ein', required: 'required'});
        var url         = inputField(inputs.companyWebsite,   {label: 'Website URL', type: 'url', name: 'url', placeholder: 'http://'});
        var payAddr     = inputField(inputs.paymentAddress,   {label: 'PayPal address', type: 'email', name: 'payAddr', placeholder: 'donations@charity.org'});
        //var firstName   = inputField(pc.firstName,        {label: 'First Name', type: 'text', name: 'firstName', required: 'required'});
        //var lastName    = inputField(pc.lastName,         {label: 'Last Name', type: 'text', name: 'lastName', required: 'required'});
        //var phoneNumber = inputField(pc.phone,            {label: 'Phone Number', type: 'text', name: 'phoneNumber', placeholder: '(xxx) xxx-xxxx'});
        //var email       = inputField(pc.email,            {label: 'Email', type: 'email', name: 'email', placeholder: 'abc@charity.org'});
        //var checkbox    = Core.input({type: 'checkbox', width: 200}, 'I agree to the terms above');

        function setVal (name, value, rv) { 
            rv.value = value; 
            return rv; 
        }

        var buttonText;
        if (false) { // (Charity.Right) {
            inputs = JsonForm.map(schema, Charity.Right, inputs, setVal);
            buttonText = 'Update';
        } else {
            buttonText = 'Register!';
        }
        var register = Core.button({text: buttonText, loud: true});

        var form = Tag.form({style: {counterReset: 'fieldsets', width: '800px', height: '720px'}}, [
                fieldset([legend('Organization Information'), ein, name, url, payAddr ]),
                //fieldset([legend('Point of Contact'), firstName, lastName, phoneNumber, email ]),
                fieldset([legend('Interchange Fee'), Tag.div({style: {left: '30px', position: 'absolute'}}, [Core.h4('3.9%')])]),
                fieldset([legend(['Terms of Agreement']), toaDiv/*, checkbox*/ ]),
                register
        ]);

        register.addEventListener('click', function (e) {
            e.preventDefault();
            var values = JsonForm.map(schema, inputs, {}, JsonForm.toVal);
            var dataString = JSON.stringify(values);
            post('/charity/update', dataString, function(data) {
                var dataString = JSON.stringify(data);
                console.log(dataString);
            });
        });


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

    define( Nav.frame(body()) );
}

require(deps, onReady);

