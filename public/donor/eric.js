var deps = [
    '../tag/tag.js',
];

var user = {
    firstName: 'Eric',
    lastName: 'Mills',
    image: '../images/eric.jpg',
    raised: '$1,040,700',
    description: 'I research cost-effective ways to save lives.  Align with me to help deliver mosquito nets, immunize children, and prevent the spread of HIV.',
};

function onReady(E) {
    function body() {
        return E.div([
            E.link({type: "text/css", href: "../css/960.css", rel: "stylesheet"}),
            E.link({type: "text/css", href: "../css/main.css", rel: "stylesheet"}),
            E.div({class: 'grid_8 widget'}, [
                E.div({class: 'influential-box'}, [
                    E.div({class: 'photo'}, [
                        E.img({src: user.image, alt: user.firstName + ' ' + user.lastName}),
                    ]),
                    E.h3({class: 'name'}, [user.firstName + ' ' + user.lastName]),
                    E.h4(['Helped raise $1,040,700']),
                    E.div({class: 'desc'}, [user.description]),
                    E.div([
                        E.a({href: '#'}, ['Align With Me']),
                        E.a({href: '#'}, ['See Other Influential Donors']),
                    ]),
                ]),
            ]),
        ]);
    }
    return {
        body: body,
    };
}

return {
    deps: deps,
    callback: onReady,
};

