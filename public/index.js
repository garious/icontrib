var deps = [
    'nav/index.js', 
    'widgets/indexbody.js',
];

function onReady(NAV, IDX) {
    return {
        title: "IContrib - Improve the world today.",
        main: NAV.frame({base: 'nav/'}, [
            IDX.body()
        ]),
    };
};

return {
    deps: deps,
    callback: onReady,
};

