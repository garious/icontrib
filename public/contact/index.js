var deps = [
    '../tag/tag.js', 
    '../nav/index.js', 
];

function onReady(E, NAV) {

    function body() {
        return E.div({id: "content", 'class': 'container_12'}, [
            E.div({'class': 'grid_4 widget alpha'}, [
                E.div({'class': 'widgetContent'}, [
                    E.h4(['Subscribe to our mailing list'])
                ])
            ]),
            E.div({'class': 'grid_4 widget'} , [
                E.div({'class': 'widgetContent'}, [
                    E.h4(['Subscribe to our RSS Feed'])
                ])
            ]),
            E.div({'class': 'grid_4 widget omega'}, [
                E.div({'class': 'widgetContent'}, [
                    E.h4(['Subscribe to the development forum']),
                    E.form({action: 'http://groups.google.com/group/icontrib-techies/boxsubscribe'}, [
                        E.div([
                            'Email: ', 
                            E.input({type: 'email', name: 'email', required: 'required'})
                        ]),
                        E.input({type: 'submit', name: 'sub', value: 'Subscribe'})
                    ]),
                    E.a({href: 'http://groups.google.com/group/icontrib-techies'}, ['Visit this group'])
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

return {
    deps: deps,
    callback: onReady
};

