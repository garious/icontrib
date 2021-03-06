var deps = [
    '/donor/checkUser.json',
    '/stdlib/dom.js',
    '/stdlib/layout.js',
    '/stdlib/observable.js',
    '/skin/frame.js',
    '/skin/core.js',
    '/skin/donor.js',
    '/skin/chart.js',
    '/skin/slider.js',
    '/skin/colors.js',
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

function onReady(auth, dom, layout, observable, frame, core, donor, chart, slider, colors, popular) {

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
                        inputs[i].set(n);
                    }
                }
            }
        }

        var rowBorder = observable.observe('1px solid white');
        var deleteObs = observable.observe('hidden');

        function onMouseOver(evt) {
            rowBorder.set('1px solid ' + colors.gray);
            deleteObs.set('visible');
        }

        function onMouseOut(evt) {
            rowBorder.set('1px solid white');
            deleteObs.set('hidden');
        }

        var rowStyle = {width: '100%', borderRadius: '10px', border: rowBorder, padding: '5px'};
        var rowChildren = [];
        var row = dom.element({name: 'div', style: rowStyle, contents: rowChildren, handlers: {mouseover: onMouseOver, mouseout: onMouseOut}});

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

        var percentage = observable.thunk([obs], function(n){return Math.round(n * 10) / 10 + '%';});

        var deleteImg = dom.element({name: 'img', attributes: {src: 'skin/delete.png', alt: 'delete'}, style: {padding: '5px', visibility: deleteObs, borderWidth: '0px'}});
        var deleteLink = dom.element({name: 'a', attributes: {href: '#'}, contents: [deleteImg], handlers: {click: onDeleteClicked}});

        var slider;
        if (navigator.userAgent.indexOf("Firefox")!=-1) {
             slider = slider.slider({value: obs, width: 200, height: 4, color: colorObs, marginTop: 10, marginBottom: 10, marginLeft: 10, marginRight: 10, onChange: onRangeChange});
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
             slider = dom.element({
                 name: 'input',
                 attributes: {type: 'range', value: obs, min: 1, max: 99},
                 style: rangeStyle,
                 handlers: {change: onRangeChange}
             });
        }

        var cols = layout.hcat([
            core.hyperlink({url: '/charity?id=' + x.cid, text: x.name, marginTop: 6, marginRight: 10}),
            slider,
            layout.gap(10),
            core.input({type: 'text', size: 5, disabled: true, value: percentage}),
            layout.gap(10),
            deleteLink
        ]);

        var rightCol = dom.element({name: 'div', style: {cssFloat: 'right'}, contents: [cols]});

        rowChildren.push(rightCol);
        return row;
    }

    function fundContents(dist, inputs, colors) {
        var rows = [];
        var rowsObs = observable.observe(rows);
        var colorAttrs = [];
        for (var j = 0; j < dist.length; j += 1) {
            var colorObs = observable.observe(colors[j % colors.length]);
            colorAttrs.push(colorObs);
            var row = fundRow(dist[j], rowsObs, rows, inputs[j], colorObs, colors, dist, inputs, colorAttrs);
            rows.push(row);
        }

        return layout.vcat({align: 'right'}, rowsObs);
    }

    function distributionTable(dist, inputs, colors) {
        return layout.hcat({width: 550}, [layout.gap(30), fundContents(dist, inputs, colors)]);
    }

    function dashboard(as) {
        var rows = [];
        var inputs = [];
        var dist = as.user.distribution;

        var total = 0;
        for (var i = 0; i < dist.length; i++) {
            var d = dist[i];
            total = total + d.shares;
        }

        for (var j = 0; j < dist.length; j += 1) {
            var x = dist[j];
            var pct = x.shares * 100 / total;
            var obs = observable.observe(pct);
            inputs.push(obs);
        }

        var pie = chart.pie({distribution: inputs, height: 220, padding: 15, colors: colors.dashboardColors});
        var pieTin = dom.element({name: 'div', style: {margin: 'auto 0px', width: '100%', textAlign: 'center'}, contents: [pie]});

        if (dist.length > 0) {
            rows.push( pieTin );
            rows.push( layout.gap(20) );
        }

        function saveChanges() {
            // Update the database
            frame.post('/donor/update', {distribution: dist}, function (dat) {
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

        rows.push( distributionTable(dist, inputs, colors.dashboardColors) );

        var buttons = layout.hcat({width: 100}, [
            core.button({href: '#', text: 'Cancel', quiet: true, onClick: cancelChanges}),
            layout.gap(10),
            core.button({href: '#', text: 'Save Changes', loud: true, onClick: saveChanges})
        ]);

        rows.push( buttons );

        return layout.vcat(rows);
    }

    var dash;
    if (auth.Right) {
        dash = dashboard({user: auth.Right});
    } else {
        dash = core.hyperlink({text: 'Please log in to modify your charitable distribution.', url: auth.Left.loginUrl});
    }

    var main = frame.frame({
        contents: layout.vcat([
            layout.hcat([
                core.box({
                    width: 600,
                    contents: dash
                }),
                layout.gap(20),
                donor.recommendedFunds({funds: popular})
            ]),
            layout.gap(20)
        ]),
        auth: auth
    });

    define(main);
}

require(deps, onReady);

