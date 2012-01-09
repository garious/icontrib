return {

    deps: [
        '../tag/tag.js', 
        '../js/less-1.1.5.min.js', // TODO: purge less
        '../jquery/jquery-mod.js'
    ],
    callback: function(E, LESS, $) { 

        var nav = function(as) {
            as = as || {};
            as.accentColor = as.accentColor || '#eeeeee';
    
            var headerStyle = {
                position: 'fixed',
                top: 0,
                left: 0,
                width: '100%',
                height: '75px',
                backgroundColor: '#000044', //2A2A8E
                zIndex: 10
            };
            
            var taglineStyle = {
                fontSize: '1.75em',
                color: as.accentColor,
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
    
            var loginForm = E.stylize({'margin-bottom': "0px"}, E.form([
                "Email ",    E.input({type: "text", name: "email", size: "10"}),
                "Password ", E.input({type: "password", name: "password", size: "10"}),
                E.input({type: 'submit', value: 'Log in or Sign up'}),
                errorBox
            ]));
    
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
    
            $.ajax({
                type: "GET",
                url: "/check_user",
                dataType: "json",
                success: function(data) {
                   loginForm.innerHTML = data;
                }
            });
    
            return E.stylize(headerStyle, E.div({class: 'container_12'}, [ 
    
                E.div({class: "grid_6"}, [
                    E.stylize(taglineStyle, E.div([
                        'Improve the world today.'
                    ])),
                    E.a({href: '../'}, [
                        E.img({src: "../images/logo4.png", alt: "IContrib Home", height: "100%", border: "0"}),
                    ]),
                ]),
    
                E.stylize(navStyle, E.div({class: "grid_6"}, [
                    loginForm,
                ]))
            ]));
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
            return E.div({class: 'footer'}, [E.div({class: 'navBar'}, xs)]);
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
                dockItem({href: '../',         src: '../images/home.png', title: "Home"}),
                dockItem({href: '../donor/',   src: '../images/portfolio.png', title: "Your Portfolio"}),
                dockItem({href: '../charity/', src: '../images/link.png', title: "Charities"}),
                dockItem({href: '../contact/', src: '../images/rss.png', title: "Keep Informed"}),
            ]);
    
            return E.div([navbar].concat(xs, [doc]));
        };
    
        return {
            nav: nav,
            dock: dock,
            dockItem: dockItem,
            frame: frame,
        };
    
    },
};
 
