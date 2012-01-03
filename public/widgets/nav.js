return YOINK.module([

    '../tag/tag.js', 
    '../js/less-1.1.5.min.js', // TODO: purge less
    '../jquery/jquery-1.7.1.min.js',     // TODO: purge jquery
    '../jquery/jquery.jqDock.min.js',

], function(E, JQUERY, LESS, DOCK) { 

    var nav = function(root, palette) {
        root = root || './'
        palette = palette || {};
        palette.accentColor = palette.accentColor || '#eeeeee';

        var headerStyle = {
            position: 'fixed',
            top: 0,
            left: 0,
            width: '100%',
            height: '100px',
            backgroundColor: '#000044',
            zIndex: 10,
        };
        
        var taglineStyle = {
            fontSize: '1.75em',
            color: palette.accentColor,
            position: 'absolute',
            left: '125px',
            top: '25px',
        };

        var navStyle = {
            backgroundColor: '#EEE',
            border: 'solid 1px silver',
            padding: '5px',
            position: 'absolute',
            right: '25px',
            top: '25px',
        };

        var errorBox = E.div();

        var loginForm = E.form([
            "Email ",    E.input({type: "text", name: "email", size: "10"}),
            "Password ", E.input({type: "password", name: "password", size: "10"}),
            E.input({type: 'submit', value: 'Log in or Sign up'}),
            errorBox,
        ]);

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
                },
            });
        };

        $.ajax({
            type: "GET",
            url: root + "check_user",
            dataType: "json",
            success: function(data) {
               loginForm.innerHTML = data;
            },
        });

        return E.stylize(headerStyle, E.div({class: 'container_12'}, [
            // TODO: purge stylesheets
            E.link({type: "text/css", href: root + "css/960.css", rel: "stylesheet"}),

            E.div({class: "grid_6"}, [
                E.stylize(taglineStyle, E.div([
                    'Improve the world today.'
                ])),
                E.a({href: root}, [
                    E.img({src: root + "images/logo4.png", alt: "IContrib Home", height: "100", border: "0"}),
                ]),
            ]),

            E.stylize(navStyle, E.div({class: "grid_6"}, [
                loginForm,
            ])),
        ]));
    };


    var dock = function(attrs) {
        var item = function(href, imgSrc, title) {
            return E.a({href: href}, [ E.img({src: imgSrc, alt: title, title: title}) ]);
        };

        var navDiv = E.div({id: 'navBar'}, [
            item('/widgets/indexbody.html', 'images/home.png', "Home"),
            item('/widgets/donorbody.html', 'images/portfolio.png', "Your Portfolio"),
            item('/widgets/charitybody.html', 'images/link.png', "Charities"),
            item('/widgets/contactbody.html', 'images/rss.png', "Keep Informed"),
        ]);

        // TODO: There's a race condition here.  Looks like jqDock requires the DOM to be ready before it can be created.
        $(navDiv).jqDock( { align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300 } );
   
        return E.div({id: 'footer'}, [navDiv]);
    };

   return {
       nav: nav,
       dock: dock,
   };

});

