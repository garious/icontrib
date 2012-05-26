var deps = [
    '/donor/checkUser.json',
    '/Tag/Interface.js', 
    '/Tag/Tag.js', 
    '/Tag/Layout.js', 
    '/Tag/Observable.js', 
    '/Skin/Frame.js',
    '/Skin/Core.js',
    '/Skin/Donor.js',
    '/Skin/Chart.js',
    '/Skin/Slider.js',
    '/Skin/Colors.js',
    '/charity/popular.json'
];

function removeItem(xs, x) {
    for (var i = 0; i < xs.length; i += 1) {
        if (xs[i] === x) {
             xs.splice(i, 1);
             return xs;
        }
    }
    return xs;
}

function onReady(Auth, Iface, Tag, Layout, Observable, Frame, Core, Donor, Chart, Slider, Colors, Popular) { 

    function fundRow(x, rowsObs, rows, obs, colorObs, colors, dist, inputs, colorAttrs) {

        function onRangeChange(evt) {
            var n = parseFloat(evt.target.value);
            if (n !== NaN && n < 100) {
                var old = x.shares;
                var diff = n - old;
                x.shares = n;

                for (var i = 0; i < inputs.length; i += 1) {
                    var d = dist[i];
                    colorAttrs[i].set( colors[i % colors.length] );
                    if (d !== x) {
                        var v = d.shares - diff * d.shares/(100 - old);
                        d.shares = v;
                        inputs[i].set(v);
                    } else {
                        inputs[i].set(evt.target.value);
                    }
                }
            }
        }

        var rowBorder = Observable.observe('1px solid white');
        var deleteObs = Observable.observe('hidden');

        function onMouseOver(evt) {
            rowBorder.set('1px solid ' + Colors.gray);
            deleteObs.set('visible');
        }

        function onMouseOut(evt) {
            rowBorder.set('1px solid white');
            deleteObs.set('hidden');
        }

        var rowStyle = {width: '100%', borderRadius: '10px', border: rowBorder, padding: '5px'};
        var rowChildren = [];
        var row = Tag.tag({name: 'div', style: rowStyle, contents: rowChildren, handlers: {mouseover: onMouseOver, mouseout: onMouseOut}});

        function onDeleteClicked(evt) {
            evt.preventDefault();

            // Remove this item from the distribution
            removeItem(inputs, obs);
            removeItem(dist, x);
            removeItem(rows, row);
            removeItem(colorAttrs, colorObs);
            rowsObs.set(rows);
            onRangeChange({target: {value: '0'}});
        }
   
        var percentage = Observable.thunk([obs], function(n){return Math.round(n * 10) / 10 + '%';});

        var deleteImg = Tag.tag({name: 'img', attributes: {src: 'Delete.png', alt: 'delete'}, style: {padding: '5px', visibility: deleteObs, borderWidth: '0px'}});
        var deleteLink = Tag.tag({name: 'a', attributes: {href: '#'}, contents: [deleteImg], handlers: {click: onDeleteClicked}});

        var slider;
        if (navigator.userAgent.indexOf("Firefox")!=-1) {
             slider = Slider.slider({value: obs, width: 200, height: 4, color: colorObs, marginTop: 10, marginBottom: 10, marginLeft: 10, marginRight: 10, onChange: onRangeChange});
        } else {
             var rangeStyle = {
                 WebkitAppearance: 'none',
                 width: '150px',
                 margin: '10px',
                 marginTop: '15px',
                 backgroundColor: colorObs,
                 height: '4px'
             };

             // At the time of this writing, this is only expected to work in Chrome, Safari, and Opera.
             slider = Tag.tag({
                 name: 'input',
                 attributes: {type: 'range', value: obs, min: 1, max: 99},
                 style: rangeStyle,
                 handlers: {change: onRangeChange}
             });
        }

        var cols = Layout.hug([
            Core.hyperlink({url: '/Charity?id=' + x.cid, text: x.name, marginTop: 6, marginRight: 10}),
            slider,
            Layout.pillow(10, 0),
            Core.input({type: 'text', size: 5, disabled: true, value: percentage}),
            Layout.pillow(10, 0),
            deleteLink
        ]);

        var rightCol = Tag.tag({name: 'div', style: {'float': 'right', cssFloat: 'right'}, contents: [cols]});

        rowChildren.push(rightCol);
        return row;
    }

    function fundContents(dist, inputs, colors) {
        var rows = [];
        var rowsObs = Observable.observe(rows);
        var colorAttrs = [];
        for (var j = 0; j < dist.length; j += 1) {
            var colorObs = Observable.observe(colors[j % colors.length]);
            colorAttrs.push(colorObs);
            var row = fundRow(dist[j], rowsObs, rows, inputs[j], colorObs, colors, dist, inputs, colorAttrs);
            rows.push(row);
        }

        return Layout.spoon({align: 'right'}, rowsObs);
    }

    function distributionTable(dist, inputs, colors) {
        return Layout.hug({width: 550}, [Layout.pillow(30), fundContents(dist, inputs, colors)]);
    }

    function dashboard(as) {
        var user = as.user;
        var rows = [];
        var inputs = [];
        var dist = user.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];
            var pct = x.shares * 100 / total;
            var obs = Observable.observe(pct);
            inputs.push(obs);
        }

        var colors = Colors.dashboardColors;
        var pie = Chart.pie({distribution: inputs, height: 220, padding: 15, colors: colors});
        var pieTin = Tag.tag({name: 'div', style: {margin: 'auto 0px', width: '100%', textAlign: 'center'}, contents: [pie]});

        if (user.distribution.length > 0) {
            rows.push( pieTin );
            rows.push( Layout.pillow(0, 20) );
        }

        function saveChanges() {
            // Update the database
            Frame.post('/donor/update', {distribution: dist}, function (dat) {
                var data = JSON.parse(dat);
                if (data.Left) {
                    alert(data.Left);
                } else {
                    // TODO: go to confirmation window
                    window.location = '/';
                }
            });
        }

        function cancelChanges() {
	    window.location.reload(true);
        }

        rows.push( distributionTable(dist, inputs, colors) );

        var buttons = Layout.hug({width: 100}, [
            Core.button({href: '#', text: 'Cancel', quiet: true, onClick: cancelChanges}),
            Layout.pillow(10, 0),
            Core.button({href: '#', text: 'Save Changes', loud: true, onClick: saveChanges})
        ]);

        rows.push( buttons );

        return Layout.spoon(rows);
    }

    var dash;
    if (Auth.Right) {
        dash = dashboard({user: Auth.Right});
    } else {
        dash = Core.hyperlink({text: 'Please log in to modify your charitable distribution.', url: '/LogIn'});
    }

    var main = Frame.frame({
        contents: Layout.spoon([
            Layout.hug([
                Core.box({
                    width: 600,
                    contents: dash
                }),
                Layout.pillow(20),
                Donor.recommendedFunds({funds: Popular})
            ]),
            Layout.pillow(20) 
        ]),
        auth: Auth
    });

    Yoink.define(main);
}

Yoink.require(deps, onReady);
 
