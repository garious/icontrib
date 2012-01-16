var deps = [
    '../tag/tag.js', 
    '../tag/layout.js', 
    '../ui/nav.js', 
    '../ui/core.js', 
    'body.html',
    'toa.html'
];

function onReady(E, L, NAV, CORE, html, toaHtml) {
    
    function body() {
        var toaDiv = E.div({'class': 'widgetContent', style: {height: '300px', overflow: 'auto'}});
        toaDiv.innerHTML = toaHtml;

        var toaFieldSet = E.fieldset([
            E.legend(['Terms of Agreement']),
            E.div({'class': 'widget'}, [toaDiv])
        ]);

        var button = E.input({type: 'submit', value: 'Register'});

        var form = E.form({counterReset: 'fieldsets', style: {width: '600px'}});
        form.innerHTML = html;
        form.appendChild(toaFieldSet);
        form.appendChild(button);

        return L.hug([
            L.pillow(300),
            L.spoon([
                E.link({type: "text/css", href: "/css/main.css", rel: "stylesheet"}),
                CORE.box([
                    E.div({style: {width: '600px'}}, [
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

