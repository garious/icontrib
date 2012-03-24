var PRELOADED_MODULES = {
"/index.js": (function (baseUrl, define, require, params) {"use strict";
var initialDeps = [
    '/donor/mostInfluential.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js', 
    '/ui/core.js', 
    '/charity/popular.json', 
    '/ui/donor.js'
];

function onInitialReady(MostInfluentialId) {

    function onReady(Tag, Layout, Nav, Core, PopularCharities, Donor, MostInfluential) {

        var mostInfluential = Core.box({
            width: 600,
            contents: Donor.profile({user: MostInfluential})
        });

        var sep = Layout.pillow(20);

        var body = Layout.spoon([
            Layout.hug([
                mostInfluential,
                sep,
                Donor.recommendedFunds({funds: PopularCharities})
            ]),
            sep,
            Nav.footer([
                Core.hyperlink({url: 'charitySignUp/', text: 'Charity Registration'})
            ])
        ]);

        define( Nav.frame([body]) );

    }

    deps.push('/donor/' + MostInfluentialId + '.json');
    require(deps, onReady);
}

require(initialDeps, onInitialReady);


}),
"/charity/index.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/core.js', 
    '/ui/nav.js',
    params.id + '.json'
];

function onReady(Tag, Layout, Core, Nav, User) {
    
    function charity(as) {
        as = as || {};
        var user = as.user;
	var box = Core.box({
            contents: Layout.spoon([
	        Tag.div({style: {height: '30px'}}, [Core.h2(user.organizationName)]),
                Layout.pillow(20),
                Layout.hug([
                    Tag.img({style: {width: '175px', height: '175px', borderRadius: '5px'}, src: user.imageUrl, alt: user.organizationName}),
                    Layout.pillow(30),
                    Layout.spoon([
                        Tag.p({style: {height: '100px', width: '600px'}}, user.mission), 
                        Layout.pillow(20),
                        Core.button({href: '/me/?donateTo=' + user.id, text: 'Donate!', loud: true})
                    ])
                ])
            ])
        });

        return Layout.spoon([
           box,
           Layout.pillow(30)
        ]);
    }

    var main = Nav.frame([charity({user: User})]);

    define(main);

}

require(deps, onReady);


}),
"/charitySignUp/index.js": (function (baseUrl, define, require, params) {"use strict";
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

    define( Nav.frame([body()]) );
}

require(deps, onReady);


}),
"/js/console.js": (function (baseUrl, define, require, params) {"use strict";
//Debugging methods - log information to console (if available)
if (!window.console) {
    console = {};
}

console.log = console.log || function () { };
console.warn = console.warn || function () { };
console.error = console.error || function () { };
console.info = console.info || function() { };

define(null);


}),
"/js/jsonform.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js'
];

function onReady(E) {
    var map = function mapObject (type, input, output, func) {
        for(var prop in type) {
            if(type.hasOwnProperty(prop)) {
                if(type[prop] === null) {
                    if(output[prop]) {
                        output[prop] = func(prop, input[prop], output[prop]);
                    } else {
                        output[prop] = func(prop, input[prop], null);
                    }
                } else if(typeof(type[prop]) == "object") {
                    if(output[prop]) {
                        output[prop] = mapObject(type[prop], input[prop], output[prop], func);
                    } else {
                        output[prop] = mapObject(type[prop], input[prop], {}, func);
                    }
                } 
            }
        }
        return output;
    };
    var toVal = function(name, val, oval) {
        return val.value;
    };
    var toInput = function (name, val, oval) { 
        return E.input({}); 
    };
    define({
        map: map,
        toInput: toInput,
        toVal: toVal
    });
}

require(deps, onReady);


}),
"/me/index.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js', 
    '/ui/nav.js',
    '/ui/core.js',
    '/ui/donor.js',
    '/ui/chart.js',
    '/charity/popular.json'
];

function onReady(Tag, Layout, Nav, Core, Donor, Chart, Popular) { 

    function fundContents(pie) {
        var dist = pie.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        var rows = [];
        var inputs = [];

        function mkHandler(j) {
            return function(evt) {
                var n = parseFloat(evt.target.value);
                if (n !== NaN && n > 0 && n < 100) {
                    var old = pie.distribution[j].shares;
                    var diff = n - old;
                    pie.distribution[j].shares = n;

                    for (var i = 0; i < inputs.length; i += 1) {
                        if (i !== j) {
                            var d = dist[i];
                            var v = d.shares - diff * d.shares/(100 - old);
                            d.shares = v;
                            var pct = Math.round(v * 100) / 100;
                            inputs[i].value = pct; 
                        }
                    }
                    pie.draw();
                } //else {
                    // TODO: disable 'Save Changes'
                //}
            };
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];

            var pct = Math.round(x.shares / total * 1000) / 10;
            var e = Core.input({type: 'text', size: 4, value: pct, onKeyUp: mkHandler(j)});

            inputs.push(e);
            var cols = Layout.hug([
                e,
                Layout.pillow(10, 0),
                Core.hyperlink({url: x.url, text: x.name})
            ]);
            rows.push(cols);
            rows.push(Layout.pillow(0,15));
        }
        return Layout.spoon(rows);
    }

    function distributionTable(pie) {
        return Layout.hug({width: 550}, [Layout.pillow(30), fundContents(pie)]);
    }

    function dashboard(as) {
        as = as || {};
        var user = as.user || {};
        var alignedUsers = user.alignedUsers || [];
        var raised = Math.round(user.alignedDonated / 100);
        var impactMsg = alignedUsers.length + ' donors are aligned with your distribution.  Together you help raise $' + raised + ' per month!';

        var rows = [];

        // TODO: clean up calculation of size of string that wraps over serveral lines
        var impactHeader = Core.h6(impactMsg);
        impactHeader.style.width = '550px';
        impactHeader.style.height = '50px';

        if (alignedUsers.length > 0) {
            rows.push( Core.h3('My impact') );
            //rows.push( Layout.hug([Layout.pillow(30), Core.h6(impactMsg)]) );
            rows.push( Layout.hug([Layout.pillow(30), impactHeader]) );
        }

        var pie = Chart.pie(user);

        if (user.distribution.length > 0) {
            rows.push( Core.h3('My charitable distribution') );
            rows.push( Layout.hug([Layout.pillow(100), pie.element]) );
        }

        var fundingRows = [
            distributionTable(pie),
            Core.h3('My funding'),
            Layout.pillow(0, 20),
            Layout.hug([Layout.pillow(30, 0), Core.input({type: 'text', size: 10, value: user.centsDonated / 100.0}), Layout.pillow(10,0), Core.h6("dollars per month")]),
            Layout.pillow(0, 20),
            Layout.hug([Layout.pillow(20,0), Core.button({href: '#', text: 'Save Changes', loud: true})]),
            Layout.pillow(0, 20)
        ];

        return Layout.spoon(rows.concat(fundingRows));
    }

    var main = Nav.frame([
        Layout.spoon([
            Layout.hug([
                Core.box({
                    width: 600,
                    contents: dashboard({user: Nav.userInfo()})
                }),
                Layout.pillow(20),
                Donor.recommendedFunds({funds: Popular})
            ]),
            Layout.pillow(20) 
        ])
    ]);

    define(main);
}

require(deps, onReady);
 

}),
"/signup/index.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js',
    '/ui/nav.js'
];

function onReady(E, NAV) {

    function body() {
        return E.div([
            E.div(['Sign up, improve the world today.']),
            E.div([
                E.form({method: "post", action: "/addUser"}, [
                   'Name:',     E.br(), E.input({type: 'text', length: 10, name: 'nameS'}), E.br(),
                   'Email:',    E.br(), E.input({type: 'text', length: 10, name: 'emailS'}), E.br(),
                   'Password:', E.br(), E.input({type: 'password', length: 10, name: 'passwordS'}), E.br(),
                   'Re-type Password:', E.br(), E.input({type: 'password', length: 10, name: 'password_reS'}), E.br(),
                   E.input({type: 'submit', value: 'Submit'})
                ])
            ]),
            E.p({align: 'center', sytle: 'font-size:24px; font-weight:bold;'}, [E.br(), E.br(), E.br(), E.br(), E.br(), 'or']),
            E.div([
                E.br(), E.br(), E.br(), E.br(),
                E.img({src: 'login-cloud.png', alt: 'Log in up above...'})
            ])
        ]);
    }

    define( NAV.frame([body()]) );
}

require(deps, onReady);


}),
"/tag/2d.js": (function (baseUrl, define, require, params) {"use strict";
// Definition of the 2D interface

define({
    TwoDimensional: {}
});


}),
"/tag/interface.js": (function (baseUrl, define, require, params) {"use strict";
//
// Interface-oriented programming for JavaScript
//
function getInterface(obj, iface) {
     var ifaces = obj && obj.constructor && obj.constructor.interfaces;
     if (ifaces) {
         for (var i = 0; i < ifaces.length; i += 1) {
             var x = ifaces[i];
             if (x['interface'] === iface) {
                 return x.instance;
             }
         }
     }
     return undefined;
}

//  //
//  // Create an object that implements only this interface
//  //
//  // Note: This is considerably more expensive than using the interface implementation directly
//  //
//  function bindInterface(obj, iface) {
//      var ifaceObj = {};
//      for (var k in iface) {
//          if (iface.hasOwnProperty(k)) {
//              ifaceObj[k] = function () {
//                  return iface[k](obj, arguments);
//              };
//          }
//      }
//      return ifaceObj;
//  }
//  
//  //
//  // If this object implements the requested interface, return an object that implements only that interface.
//  //
//  function queryInterface(obj, iface) {
//       var x = getInterface(obj, iface);
//       return x && bindInterface(obj, x);
//  }


define({
    getInterface: getInterface
});

}),
"/tag/layout1.js": (function (baseUrl, define, require, params) {"use strict";
//
// Layout with hugging and spooning
//

// All combinators are of type "Maybe attrs -> Array a -> a"
//
// hug(  ['a','b','c'])       === 'abc'
// spoon(['a','b','c'])       === 'a\nb\nc'

var deps = [
    'interface.js',
    '2d.js',
    'todom.js'
];

// TODO: Move this into Tag
var Tag_TwoDimensional = {

    // Calculate outer width of a DOM element
    getDimensions: function (me) {
        var sty = me.style;

        var width  = parseInt(sty.width,  10) || 0;
        var height = parseInt(sty.height, 10) || 0;

        width  += parseInt(sty.marginLeft,   10) || 0;
        width  += parseInt(sty.marginRight,  10) || 0;
        height += parseInt(sty.marginTop,    10) || 0;
        height += parseInt(sty.marginBottom, 10) || 0;

        width  += parseInt(sty.paddingLeft,   10) || 0;
        width  += parseInt(sty.paddingRight,  10) || 0;
        height += parseInt(sty.paddingTop,    10) || 0;
        height += parseInt(sty.paddingBottom, 10) || 0;

        width  += parseInt(sty.borderLeftWidth,   10) || 0;
        width  += parseInt(sty.borderRightWidth,  10) || 0;
        height += parseInt(sty.borderTopWidth,    10) || 0;
        height += parseInt(sty.borderBottomWidth, 10) || 0;

        return {
            width:  width,
            height: height
        };
    },

    setPosition: function (me, pos) {
        me.style.position = 'absolute';
        me.style.top = pos.top + 'px';
        me.style.left = pos.left + 'px';
        return me;
    }
    
};

var Tag_ToDom = {
    toDom: function (me) {
        return me;
    },
    getTitle: function (me) {
        return undefined;
    }
};


function onReady(I, DIM, DOM) {

    // a TwoDimensional instance for the Pillow class
    var Pillow_TwoDimensional = {
        getDimensions: function (me) {
            return {
                width: me.width,
                height: me.height
            };
        },

        setPosition: function (me, pos) {
            return me;
        }
    };
    
    // a TwoDimensional instance for the Party class
    var Party_TwoDimensional = Pillow_TwoDimensional;
    
    // a ToDom instance for the Party class
    var Party_ToDom = {
        toDom: function (me) {
            var div = document.createElement('div');
            div.style.height = me.height + 'px';
            div.style.width = me.width + 'px';
            div.style.position = 'absolute';
    
            // ys = filter (!= pillow) xs
            var xs = me.subelements;
            for (var i = 0; i < xs.length; i += 1) {
                var x = xs[i];
                var iface = I.getInterface(x, DOM.ToDom) || Tag_ToDom;
                x = iface && iface.toDom(x) || x;
                if (x.constructor !== pillow) {  // Since DOM elements to not implement ToDom, we unfortunately have to pull pillows explicitly.
                    div.appendChild(x);
                }
            }
            return div;
        },
        getTitle: function (me) {
            return undefined;
        }
    };
    
    // pillow(w, h)
    //
    //     Create empty space of 'w' pixels wide and 'h' pixels tall.  Pillow elements 
    //     are not added to the DOM, and are only used for managing space.
    function pillow(w, h) {
        if (h === undefined) {
            h = w;
        }
        return {
            constructor: pillow,
            width: w,
            height: h 
        };
    }
    
    pillow.interfaces = [
        {'interface': DIM.TwoDimensional, instance: Pillow_TwoDimensional}
    ];
    
    // party(attrs, subelements)
    //
    //    a placeholder for visual elements to snuggle
    function party(as, xs) {
    
        return {
            constructor: party,
            width: as.width,
            height: as.height,
            subelements: xs
        };
    }
    
    party.interfaces = [
        {'interface': DOM.ToDom,          instance: Party_ToDom},
        {'interface': DIM.TwoDimensional, instance: Party_TwoDimensional}
    ];
    
    // Concatenate elements
    function cat(as, xs, setPos) {
    
        // dim = reduce(setPos, xs, (0,0))
        var dim = {width: 0, height: 0};
        for (var i = 0; i < xs.length; i += 1) {
            var x = xs[i];
            dim = setPos(x, dim);
        }
    
        return party(dim, xs);
    }
    
    // Set the horizontal position of a 2D element
    function setHPos(x, dim) {
        var iface = I.getInterface(x, DIM.TwoDimensional) || Tag_TwoDimensional;
        iface.setPosition(x, {'top': 0, left: dim.width});
    
        var d = iface.getDimensions(x);
        return {
            width: dim.width + d.width,
            height: d.height > dim.height ? d.height : dim.height
        };
    }
    
    // Concatenate elements horizontally
    function hcat(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        return cat(as, xs, setHPos);
    }
    
    // Set the vertical position of a 2D element
    function setVPos(x, dim) {
        var iface = I.getInterface(x, DIM.TwoDimensional) || Tag_TwoDimensional;
        iface.setPosition(x, {'top': dim.height, left: 0});

        var d = iface.getDimensions(x);
        return {
            height: dim.height + d.height,
            width: d.width > dim.width ? d.width : dim.width
        };
    }
    
    // Concatenate elements vertically
    function vcat(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        return cat(as, xs, setVPos);
    }
    
    // Concatenate elements horizontally and wrap in a DOM element
    function hug(as, xs) {
        var b = hcat(as, xs);
        var iface = I.getInterface(b, DOM.ToDom);
        return iface.toDom(b);
    }
    
    // Concatenate elements vertically and wrap in a DOM element
    function spoon(as, xs) {
        var b = vcat(as, xs);
        var iface = I.getInterface(b, DOM.ToDom);
        return iface.toDom(b);
    }
    
    define({
        hcat:   hcat,
        vcat:   vcat,
        hug:    hug,
        spoon:  spoon,
        pillow: pillow
    });
}

require(deps, onReady);


}),
"/tag/tag.js": (function (baseUrl, define, require, params) {"use strict";
// Create a DOM element from a name, attributes object, and array of children.

function text(s) {
    return document.createTextNode(s);
}

function tag(nm, as, xs, es) {
    if (typeof as === 'string' || as && as.constructor === Array) {
        es = xs;
        xs = as;
        as = null;
    }

    // Add attributes
    var e = document.createElement(nm); 
    var k;
    if (as) {
        for (k in as) {
            if (as.hasOwnProperty(k)) {
                if (k === 'style') {
                    var style = as[k];
                    for (var s in style) {
                        e.style[s] = style[s];
                    }
                } else {
                    e.setAttribute(k, as[k]);
                }
            }
        }
    }

    // Add children
    if (xs) {
        if (typeof xs === 'string') {
            e.appendChild(text(xs));
        } else {
            for (var i = 0; i < xs.length; i++) {
                var x = xs[i];
                if (typeof x === 'string') {
                   x = text(x);
                }
                e.appendChild(x);
            }
        }
    }

    // Add event handlers
    if (typeof es === 'object') {
        for (k in es) {
            if (es.hasOwnProperty(k)) {
                e.addEventListener(k, es[k]);
            }
        }
    }

    return e;
}

function mkTag(nm) {
    return function(as, xs, es) {
        return tag(nm, as, xs, es);
    };
}

var Tag = {
    tag:        tag,
    mkTag:      mkTag,
    text:       text
};

var tags = [
    'br', 'hr', 'p', 'div', 'link', 'a', 'img', 'span',
    'form', 'fieldset', 'input', 'label', 'button',
    'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
    'base', 'ul', 'ol', 'li', 'legend', 
    'table', 'th', 'tr', 'td', 'thead', 'tbody', 'tfoot',
    'canvas'
];

for (var i = 0; i < tags.length; i++) {
    var nm = tags[i];
    Tag[nm] = mkTag(nm);
}

define(Tag);



}),
"/tag/todom.js": (function (baseUrl, define, require, params) {"use strict";
// Definition of the ToDom interface

var ToDom = {
    // Returns a DOM node for this object
    toDom:    function (me) {},

    // If this widget is displayed as a web page, what string do you want in the title bar?
    // Return 'undefined' to have the browser display the default title.
    getTitle: function (me) {}
};


// ToDom instance for strings
var String_ToDom = {
    toDom: function (me) {
        return document.createTextNode(me);
    },
    getTitle: function (me) {
        return undefined;
    }
};

String.interfaces = String.interfaces || [
    {'interface': ToDom, 'instance': String_ToDom}
];


define({
    ToDom: ToDom
});


}),
"/ui/chart.js": (function (baseUrl, define, require, params) {"use strict";
function exportGoogle(text, require, callback) {
    YOINK.interpreters.js(text + '\ndefine(google);', require, callback);
}

var deps = [
    '/tag/tag.js',
    'colors.js',
    {path: '/mirror/google/jsapi', interpreter: exportGoogle}
];

function onReady(Tag, Colors, Google) {


    // Uses Google visualization library to generate an interactive pie chart
    function pie(as) {
        var userChart = Tag.div({style: {width: '300px', height: '225px'}}, [
            Tag.img({src: baseUrl + '/ajax-loader.gif', alt: 'Loading...', style: {margin: '0px auto'}})
        ]);
   
        var me = {
            element: userChart,
            distribution: as.distribution
        };

        var chart = null;
        var data = null;
        var options = {width: 300, height: 225, tooltip: {trigger: 'none'}, legend: {position: 'none'}};
        options.colors = [Colors.darkColor, Colors.middleColor, Colors.lightColor];

        function draw() {
            if (data && chart) {
                 var dist = [];
                 for (var i = 0; i < me.distribution.length; i++) {
                     var ud = me.distribution[i]; 
                     dist.push([ud.name, ud.shares]); 
                 }
                 data.removeRows(0, data.getNumberOfRows());
                 data.addRows(dist);
                 chart.draw(data, options);
            }
        }

        function createChart() {
            chart = new Google.visualization.PieChart(userChart);
            data = new Google.visualization.DataTable();
            data.addColumn('string', 'Charity');
            data.addColumn('number', 'Percentage');
            draw();
        }

        Google.load('visualization', '1.0', {packages:['corechart'], callback: createChart});

        // expose the draw function to the caller
        me.draw = draw;

        return me;
    }

    // Uses Google API to generate a simple pie chart image
    function pie1(as) {
        var height = 150;
        var width = 150;
        var chs = height + 'x' + width;
        var chd = 't:';
        for (var i = 0; i < as.distribution.length; i++) {
            var x = as.distribution[i]; 
            if (i != as.distribution.length - 1) {
               chd += x.shares + ','; 
            } else {
               chd += x.shares;
            }
        }

        return Tag.img({
            src: 'https://chart.googleapis.com/chart?cht=p&chco=a7d322&chs=' + chs + '&chd=' + chd,
            alt: 'Chart',
            style: {width: width + 'px', height: height + 'px'}
        });
    }

    define({
        pie: pie,
        pie1: pie1
    });
}

require(deps, onReady);



}),
"/ui/colors.js": (function (baseUrl, define, require, params) {"use strict";
//
// Color Scheme for IContrib.org
//

var smallestHeader = 15;

define({
    lightColor:    '#979797',
    middleColor:   '#777777',
    darkColor:     '#5e5e5e',
    greenText:     '#8bb800',
    green:         '#a7d322',
    redText:       '#ff4e00',
    red:           '#ff4e00',
    grey:          '#d7d7d7',
    h1Size:        10 + smallestHeader,
    h2Size:         8 + smallestHeader,
    h3Size:         6 + smallestHeader,
    h4Size:         4 + smallestHeader,
    h5Size:         2 + smallestHeader,
    h6Size:             smallestHeader
});


}),
"/ui/core.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js',
    '/tag/layout1.js',
    'colors.js'
];

function onReady(Tag, Layout, Colors) {

    var defaultFont = "/1.5 'Helvetica Neue', Arial, 'Liberation Sans', FreeSans, sans-serif";
    var defaultFontSize = 15;
    var font = defaultFontSize + "px" + defaultFont;

    function textDimensions(as, s) {
        var canvas = Tag.canvas();
        var fontSize = as.fontSize || defaultFontSize;

        if (canvas && canvas.getContext) {

            var ctx = canvas.getContext('2d');
            ctx.font = fontSize + "px" + defaultFont;
            ctx.fontSize = fontSize;

            var dim = ctx.measureText(s);

            return {
                width: dim.width,
                height: fontSize + 6
            };

        } else {
            // No canvas available on this browser - time to guess.
            return {
                width: fontSize * s.length * 0.6,
                height: fontSize + 6
            };
        }
    }

    function hyperlink(as) {
        var dim = textDimensions({}, as.text);

        var sty = {
            textDecoration: 'none',
            font: font,
            width: dim.width + 'px',
            height: dim.height + 'px',
            color: 'blue'
        };

        var handlers = {
            mouseover: function(evt) { evt.target.style.textDecoration = 'underline'; },
            mouseout:  function(evt) { evt.target.style.textDecoration = 'none'; }
        };

        return Tag.a({style: sty, href: as.url}, [as.text], handlers);
    }

    function input(as) {
        var width  = (as.width  || as.size * 10) + 'px';
        var height = (as.height || 20) + 'px';

        var attrs = {type: as.type, size: as.size, style: {height: height, width: width}};

        // Special handling for 'value' attribute, which will awkwardly write the text "undefined".
        if (as.value !== undefined) {
            attrs.value = as.value;
        }

        var handlers = {keyup: as.onKeyUp};

        return Tag.input(attrs, null, handlers);
    }

    function button(as) {
        var dim = textDimensions({}, as.text);

        var color      = as.loud ? Colors.red : Colors.middleColor;
        var focusColor = as.loud ? Colors.red : Colors.lightColor;

        var handlers = {
            mouseover: function(evt) { evt.target.style.backgroundColor = focusColor; },
            mouseout:  function(evt) { evt.target.style.backgroundColor = color; },
            click: as.onClick
        };

        return Tag.a({
            href: as.href || '#', 
            style: {
                width: dim.width + 'px',
                height: dim.height + 'px',
                font: font, 
                textDecoration: 'none', 
                textAlign: 'center', 
                backgroundColor: color, 
                color: '#fff', 
                padding: '5px', 
                borderRadius: '2px'
            }
        }, as.text, handlers);
    }

    function box(as) {
        var shadow = '0px 0px 5px 2px #ddd';
        var e = as.contents;

        var padding = 15;
        return Tag.div({
            style: {
                border: '2px solid #cfcfcf',
                shadow: shadow,
                MozBoxShadow: shadow,
                WebkitBoxShadow: shadow,
                width:  as.width  ? (as.width - 2 * padding - 4) + 'px' : e.style.width,
                height: as.height ? (as.height - 2 * padding - 4) + 'px' : e.style.height,
                padding: padding + 'px'
            }
        }, [e]);
    }

    // Create the style attribute for HTML header elements
    function hStyle(fontSize, s) {
        var dim = textDimensions({fontSize: fontSize}, s);

        return {
            width: dim.width + 'px',
            height: dim.height + 'px',
            font: font,
            fontSize: fontSize + 'px',
            margin: 0,
            color: Colors.darkColor
        };
    }

    // Create a header constructor.  
    //
    // The returned constructor accepts either an attributes object or a string.
    //
    //     mkHeader(2)('hello!') === h2('hello!')
    //     mkHeader(3)('hello!') === h3('hello!')
    //     mkHeader(3)('hello!') === h3({text: 'hello!'})
    //
    function mkHeader(n) {

        function header(as) {
            var s = typeof as === "string" ? as : as.text;

            var sty = hStyle(Colors['h' + n + 'Size'], s); 

            if (typeof as === "object") {
                sty.color = as.color !== 'undefined' ? as.color : sty.color;
            }

            return Tag['h' + n]({style: sty}, s);
        }

        return header;
    }

    function label(s) {
        var dim = textDimensions({}, s);
        return Tag.label({style: {width: dim.width + 'px', height: dim.height + 'px', font: font}}, s);
    }

    function p(s) {
        var dim = textDimensions({}, s);
        return Tag.p({style: {width: dim.width + 'px', height: dim.height + 'px', font: font, margin: '0px'}}, s);
    }

    function hr(as) {
        as = as || {};

        var sty = {
           height: as.height ? as.height + 'px' : '1px',
           width:  as.width  ? as.width  + 'px' : '100%',
           margin: 0,
           borderWidth: 0,
           backgroundColor: as.color
        };

        return Tag.hr({style: sty, noshade: true, size: 1});
    }

    define({
         hyperlink: hyperlink,
         input: input,
         label: label,
         button: button,
         box: box,
         h1: mkHeader(1),
         h2: mkHeader(2),
         h3: mkHeader(3),
         h4: mkHeader(4),
         h5: mkHeader(5),
         h6: mkHeader(6),
         p: p,
         hr: hr,
         defaultFont: font
    });
}

require(deps, onReady);


}),
"/ui/donor.js": (function (baseUrl, define, require, params) {"use strict";
var deps = [
    '/tag/tag.js', 
    '/tag/layout1.js',
    '/ui/chart.js', 
    '/ui/colors.js', 
    '/ui/core.js'
];

function onReady(Tag, Layout, Chart, Colors, Core) {

    function alignButton(user) {
        return Core.button({href: '/me/?donateTo=' + user.id, loud: true, text: 'Donate!'});
    }

    function isMember(xs, x) {
        for (var i = 0; i < xs.length; i++) {
            if ( xs[i] === x ) {
                return true;
            }
        }
        return false;
    }

    function fundContents(xs, total) {
        var rows = [Layout.pillow(0, 15)];

        var colors = [
            Colors.green,
            '#ddffaa'
        ];

        for (var j = 0; j < xs.length; j++) {
            var x = xs[j];
            var pct = Core.h6(Math.round(1000 * x.shares / total) / 10 + '%');


            var cols = Layout.hug([
                Tag.div({style: {width: '18px', height: '18px', backgroundColor: colors[j % colors.length]}}),
                Layout.pillow(15, 0),
                Tag.div({style: {width: '55px', height: pct.height}}, [pct]),
                Core.hyperlink({url: 'charity/?id=' + x.cid, text: x.name})
            ]);
            rows.push(cols);
        }
        return Layout.spoon(rows);
    }

    function distributionTable(user) {
        if (user.funds) {
            var rows = [];
            var dist = user.distribution;
            for (var i = 0; i < user.funds.length; i++) {

                var fundId = user.funds[i].labels[0];  // TODO: Anatoly, why is this a list?
                var xs = [];
                var total = 0;

                // filter (nm `elem` dist.labels)
                for (var j = 0; j < dist.length; j++) {
                    if (isMember(dist[j].labels, fundId)) {
                        var d = dist[j];
                        total = total + d.shares;
                        xs.push(d);
                    }
                }

                var row = Layout.spoon([
                    Core.hr({width: 570}),
                    Layout.pillow(0, 20),
                    Tag.div({style: {height: '30px'}}, [
                        Core.h4(user.funds[i].name),
                        Tag.div({style: {position: 'absolute', top: '10px', left: '505px'}}, [  // TODO: remove top 10px, which is due to the button falling outside its bounds
                            alignButton({id: fundId})
                        ])
                    ]),
                    Layout.hug([
                        Chart.pie1({distribution: xs}),
                        Layout.pillow(20, 0),
                        fundContents(xs, total)
                    ])
                ]);

                rows.push(row);
            }
            return Layout.spoon(rows);
        }
    }

    function profile(as) {
        as = as || {};
        var user = as.user || {};
        var userInfo = Layout.hug([
            Layout.pillow(25, 0), 
            Layout.spoon([
                Core.h3(user.firstName + ' ' + user.lastName),
                Core.h5({
                    color: 'red',
                    text: 'Helps raise $' + Math.round(user.alignedDonated / 100) + ' per month'
                })
            ])
        ]);

        return Layout.spoon([
            Layout.hug([
                Tag.img({style: {width: '90px', height: '90px'}, src: user.imageUrl, alt: user.firstName + ' ' + user.lastName}),
	        userInfo
            ]),
            Layout.pillow(0, 10),
            distributionTable(user)
        ]);
    }

    function recommendedFunds(as) {

        var listItems = [
            Core.h5({
                color: Colors.greenText,
                text: 'Recommended Funds'
            })
        ];

        var pad = Layout.pillow(0, 10);

        for (var i = 0; i < as.funds.length; i += 1) {
            var x = as.funds[i];
            listItems.push( pad );
            listItems.push( Core.hr({width: 300}) );
            listItems.push( pad );

            var e = Layout.hug([
                Tag.img({src: x.imageUrl, style: {width: '50px', height: '50px'}}),
                Layout.pillow(20, 0),
                Core.hyperlink({url: '/charity/?id=' + x.cid, text: x.name})
            ]);

            listItems.push(e);
        }

        return Core.box({
            width: 340,
            contents: Layout.spoon(listItems)
        });
    }

    define({
        profile: profile,
        recommendedFunds: recommendedFunds,
        alignButton: alignButton
    });
}

require(deps, onReady);


}),
"/ui/nav.js": (function (baseUrl, define, require, params) {"use strict";
var authDeps = [
    '/auth/check.json'
];

var deps = [
    '/tag/tag.js', 
    '/tag/todom.js', 
    '/tag/layout1.js', 
    'core.js',
    'colors.js'
];

// TODO: Find this function a better home.
function getDimensions(me) {
    return {
        width: parseInt(me.style.width, 10),
        height: parseInt(me.style.height, 10)
    };
}


// TODO: how to get window.innerHeight in IE 8?
function getWindowInnerHeight() {
    return window.innerHeight;
}

function getWindowInnerWidth() {
    return window.innerWidth;
}

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


function onAuthReady(Auth) { 
function onReady(Tag, ToDom, Layout, Core, Colors, Me) { 

    function loginWidget(as) {
        var username = Core.input({type: 'text', size: 18});
        var password = Core.input({type: 'password', size: 18});

        function submit(evt) {
            evt.preventDefault();
            var formValues = {
                email: username.value,
                password: password.value 
            };
            post('/auth/login', formValues, function(dat) {
                var data = JSON.parse(dat);
                if(data.Left) {
                    badLogin.hidden = false;
                } else {
                    window.location = '/me/';
                }
            });
        }

        if (Auth.Left) {
            var badLogin = Tag.span({hidden: true, style: {height: '20px', width: '200px', color: 'red'}}, 'bad username or password');

            var widget = Layout.hug([
                Layout.spoon([
                    Layout.hug([Core.label('Username'), Layout.pillow(5, 0), username]),
                    Layout.pillow(0, 5),
                    Layout.hug([Core.label('Password'), Layout.pillow(5, 0), password]),
                    Layout.pillow(0, 5),
                    badLogin,
                    Layout.pillow(0, 5)
                ]),
                Core.button({text: 'Log in', onClick: submit})
            ]);

            widget.addEventListener('keyup', function(evt) {
                if (evt.keyCode === 13) {
                   submit(evt);
                }
            });

            return widget;

        } else {
            var handlers = {
                click: function(evt) {
                    evt.preventDefault();
                    post('/auth/logout', {}, function(data) {
                        window.location = '/';
                    });
                }
            };

            //var logoutButton = Core.hyperlink({url: '#', text: 'Sign out'});
            var logoutButton = Tag.img({src: baseUrl + '/arrowdown-darkgreen.png', alt: 'settings'}, null, handlers);

            return Tag.div({
                style: {
                    width: '270px',
                    height: '77px',
                    backgroundColor: '#eee',
                    borderRadius: '5px 5px 0px 0px',
                    border: '1px solid',
                    borderBottomWidth: '0px',
                    borderColor: Colors.lightColor
                }
            }, [
                Layout.spoon([
                    Layout.pillow(0, 15),
                    Layout.hug([
                        Layout.pillow(20, 0),
                        as.thumbnail,
                        Layout.pillow(20, 0),
                        Layout.spoon([
                            Layout.pillow(0, 22),
                            logoutButton
                        ])
                    ])
                ])
            ]);
        }
    }

    function nav(as) {
        as = as || {};

        var logo = Tag.a({href: '/', style: {width: '129px', height: '70px'}}, [
            Tag.img({src: baseUrl + "/logo.png", alt: "IContrib Home", border: "0"})
        ]);

        return Layout.spoon([
            Layout.pillow(0, 20),
            Layout.hug([
                logo,
                Layout.pillow(559, 0),
                loginWidget(as)
            ]),
            Core.hr({width: 960, height: 4, color: Colors.green})
        ]);
    }

    //
    // Web Page object
    //
    function webpage(domNode) {
        return {
            constructor: webpage,
            domNode: domNode
        };
    }
    var Page_ToDom = {
        toDom: function (me) {
            return me.domNode;
        },
        getTitle: function (me) {
            return "IContrib.org";
        }
    };
    webpage.interfaces = [
        {'interface': ToDom.ToDom, 'instance': Page_ToDom}
    ];

    function frame(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};

        if (Auth.Right) {
            var thumbContents = Layout.hug([
                Tag.img({style: {width: '50px', height: '50px'}, src: Me.imageUrl, alt: Me.firstName + ' ' + Me.lastName}),
                Layout.pillow(20, 0),
                Layout.spoon([
                    Layout.pillow(0, 10),
                    Core.h3({
                        color: Colors.greenText,
                        text: Me.firstName + ' ' + Me.lastName
                    })
                ])
            ]);

            var dim = getDimensions(thumbContents);

            var thumbnail = Tag.a({href: '/me/', style: {width: dim.width + 'px', height: dim.height + 'px', textDecoration: 'none'}}, [
                thumbContents
            ]);

            as.thumbnail = thumbnail;
        }

        var navbar = nav(as);
        var body = Tag.div(xs);

        var node = Tag.div({style: {margin: '0px auto', height: getWindowInnerHeight(), width: '960px'}}, [
            Layout.spoon([
                navbar, 
                Layout.pillow(50), 
                body
            ])
        ]);

        return webpage(node);
    }

    function footer(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = {};
        }
        as.style = as.style || {};
        as.style.bottom = '0px';
        as.style.width = '100%';
        as.style.textAlign = 'center';

        return Tag.div(as, [
            Core.hr(),
            Tag.div({style: {paddingRight: '20px'}}, xs)
        ]); 
    }

    function userInfo() {
        return Me;
    }

    define({
        nav: nav,
        frame: frame,
        footer: footer,
        userInfo: userInfo
    });

}

var donorId = Auth.Left && 'anonymous' || Auth.Right;
var donorUrl = '/donor/' + donorId + '.json';
deps.push(donorUrl);
require(deps, onReady);
}

require(authDeps, onAuthReady);
 

})};
