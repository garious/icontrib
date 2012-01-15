var deps = [
    '../tag/tag.js', 
    '../tag/layout.js', 
    '../css/colors.json', 
    '../jquery/jquery-mod.js',
    '../ui/core.js'
];

function onReady(E, L, C, $, CORE) { 

    var nav = function() {
        var headerStyle = {
            overflow: 'auto', // Required for IE
            top: 0,
            left: 0,
            width: '100%',
            height: '75px',
            backgroundColor: C.midDarkColor,
        };
        
        var taglineStyle = {
            fontSize: '1.75em',
            color: C.accentColor,
            position: 'absolute',
            left: '125px',
            top: '15px'
        };

        var navStyle = {
            backgroundColor: '#EEE',
            border: 'solid 1px silver',
            padding: '5px',
            position: 'absolute',
            right: '25px',
            top: '15px'
        };

        var errorBox = E.div();

        var loginForm = E.form({style: {marginBottom: 0}}, [
            "Email ",    E.input({type: "text", name: "email", size: "20"}),
            "Password ", E.input({type: "password", name: "password", size: "10"}),
            E.input({type: 'submit', value: 'Log in'}),
            errorBox
        ]);

        loginForm.onsubmit = function(e){
            e.preventDefault();
            var dataString = loginForm.serialize();
            $.ajax({
                type: "POST",
                url: "/get_user",
                data: dataString,
                dataType: "json",
                success: function(data) {
                    errorBox.innerHTML = data;
                }
            });
        };

        // TODO: this is broken?
        //$.ajax({
        //    type: "GET",
        //    url: "/check_user",
        //    dataType: "json",
        //    success: function(data) {
        //       loginForm.innerHTML = data;
        //    }
        //});

        return E.div({style: headerStyle}, [ 
            E.a({href: '/'}, [
                E.img({src: "/images/logo4.png", alt: "IContrib Home", style: {height: "100%"}, border: "0"})
            ]),

            E.div({style: taglineStyle}, [
                //'Improve the world today.'
                CORE.h2('Improve the world today.')
            ]),

            E.div({style: navStyle, 'class': "widget"}, [loginForm])
        ]);
    };

    var dockItem = function(as) {
        as = as || {};
        var e = E.a({href: as.href}, [ E.img({src: as.src, alt: as.title, title: as.title}) ]);
        return e;
    };

    var dock = function(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        return E.div({'class': 'footer'}, [E.div({'class': 'navBar'}, xs)]);
    };

    var frame = function(as, xs) {
        if (as && as.constructor === Array) {
            xs = as;
            as = null;
        }
        xs = xs || [];
        as = as || {};
        
        var navbar = nav();

        var doc = dock( /*{align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300},*/ [
            dockItem({href: '/',         src: '/images/home.png', title: "Home"}),
            dockItem({href: '/donor/',   src: '/images/portfolio.png', title: "Your Portfolio"}),
            dockItem({href: '/charity/', src: '/images/link.png', title: "Charities"}),
            dockItem({href: '/contact/', src: '/images/rss.png', title: "Keep Informed"})
        ]);

        var body = E.div({'class': 'separator'}, xs);

        return L.spoon([navbar, body/*, doc*/], 20);
    };


    var footerStyle = {
        position: 'fixed',
        bottom: '0px',
        width: '100%',
        background: C.midDarkColor,
        height: '23px',
        color: C.accentColor,
        textAlign: 'right'
    };

    function footer(xs) {
        return E.div({style: footerStyle}, [
            E.div({style: {paddingRight: '20px'}}, xs)
        ]); 
    }

    return {
        nav: nav,
        dock: dock,
        dockItem: dockItem,
        frame: frame,
        footer: footer
    };

}

return {
    deps: deps,
    callback: onReady
};
 
