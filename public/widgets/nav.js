return {

    deps: [
        '../tag/tag.js', 
        '../js/less-1.1.5.min.js', // TODO: purge less
        '../jquery/jquery-mod.js'
    ],
    callback: function(E, LESS, $) { 

        var nav = function(root, palette) {
            root = root || './'
            palette = palette || {};
            palette.accentColor = palette.accentColor || '#eeeeee';
    
            var headerStyle = {
                position: 'fixed',
                top: 0,
                left: 0,
                width: '100%',
                height: '75px',
                backgroundColor: '#000044',
                zIndex: 10
            };
            
            var taglineStyle = {
                fontSize: '1.75em',
                color: palette.accentColor,
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
                    url: root + "get_user",
                    data: dataString,
                    dataType: "json",
                    success: function(data) {
                        errorBox.innerHTML = data;
                    }
                });
            };
    
            $.ajax({
                type: "GET",
                url: root + "check_user",
                dataType: "json",
                success: function(data) {
                   loginForm.innerHTML = data;
                }
            });
    
            return E.stylize(headerStyle, E.div({class: 'container_12'}, [
                // TODO: purge stylesheets
                E.link({type: "text/css", href: root + "css/960.css", rel: "stylesheet"}),
    
                E.div({class: "grid_6"}, [
                    E.stylize(taglineStyle, E.div([
                        'Improve the world today.'
                    ])),
                    E.a({href: root}, [
                        E.img({src: root + "images/logo4.png", alt: "IContrib Home", height: "100%", border: "0"}),
                    ]),
                ]),
    
                E.stylize(navStyle, E.div({class: "grid_6"}, [
                    loginForm,
                ]))
            ]));
        };
    
        var dockItem = function(as) {
            var e = E.a({href: 'javascript: void(0);'}, [ E.img({src: as.src, alt: as.title, title: as.title}) ]);
            e.addEventListener('click', as.onclick, false);
            return e;
        };
    
        var dock = function(as, xs) {
            if (as.constructor === Array) {
                xs = as;
                as = null;
            }
    
            //var e = E.div({class: 'footer'}, [E.stylize({margin: "0 auto", width: '400px'}, E.div(xs))]);
    		var e = E.div({class: 'footer'}, [E.div({class: 'navBar'}, xs)]);
            return e;
        };
    
       return {
           nav: nav,
           dock: dock,
           dockItem: dockItem
       };
    
    },
};
 
