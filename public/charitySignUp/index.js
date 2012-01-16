var deps = [
    '../tag/tag.js', 
    '../nav/index.js', 
    'body.html',
    'toa.html'
];

function onReady(E, NAV, html, toaHtml) {
    
    function body() {
        var toaDiv = E.div({'class': 'widgetContent'});
        toaDiv.innerHTML = toaHtml;

        var toaFieldSet = E.fieldset([
            E.legend(['Terms of Agreement']),
            E.div({'class': 'widget'}, [toaDiv])
        ]);

        var button = E.input({type: 'submit', value: 'Sign up'});

        var form = E.form({'counter-reset': 'fieldsets'});
        form.innerHTML = html;
        form.appendChild(toaFieldSet);
        form.appendChild(button);

        return E.div({id: 'content', 'class': 'container_12'}, [
            E.div({id: 'call-to-action', 'class': 'grid_12'}, [
        	E.div({'class': 'widgetContent'}, ['Enable your organization to recieve regular contributions today!'])
            ]),
            E.div({'class': 'grid_12'}, [
                E.div({'class': 'widget'}, [
        	    E.div({'class': 'widgetContent'}, [form])
                ])
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

