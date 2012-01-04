
return YOINK.module([

    '../tag/tag.js', 
    '../js/drawChart.js',  // TODO: Make this a proper module

], function(E) { 

    // TODO: Why does this fail to load if I remove the 'userChart' ID?
    var userChart = E.div({id: 'userChart'}, [
        E.div({id: 'chartPlaceHolder', style: 'width: 400px; height: 300px;'}, [
            E.img({src: '/images/ajax-loader.gif', alt: 'Loading...', style: 'margine: 0px auto;'}),
        ]),
    ]);

    iContrib.initLoader(userChart);

    var defaultUser = {
       firstName: 'Greg',
       lastName: 'Fitzgerald',
       imageUrl: 'images/gregf.jpg',
       dollarsDonated: '$2,456',
       alignedDonated: '$12,456',
       alignedImageUrl: 'images/friends.png',
    };

    var body = function(user) {

        user = user || defaultUser;

        return E.div({id: 'content', class: 'container_12'}, [
            E.div({id: 'call-to-action', class: 'gid_12'}, [
                E.div({class: 'widgetContent'}, [
                    'Align with me to support underwater hockey and to make San Diego more awesome!'
                ]),
            ]),
            
            E.div({class: 'grid_3 alpha'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + ' ' + user.lastName]),
                        E.img({src: user.imageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + "'s donated " + user.dollarsDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_6 widget'}, [
                E.div({class: 'widgetContent'}, [
                    userChart,
                ]),
            ]),
        
            E.div({class: 'grid_3 omega'}, [
                E.div({class: 'widget'}, [
                    E.div({class: 'widgetContent'}, [
                        E.h3(['Aligned with ' + user.firstName]),
                        E.img({src: user.alignedImageUrl, height: '170px', width: '170px'}),
                    ]),
        
                    E.div({class: 'widgetContent'}, [
                        E.h3([user.firstName + "'s friends have raised " + user.alignedDonated]),
                    ]),
                ]),
            ]),
        
            E.div({class: 'grid_12 clear bread'}),
        ]);
    };

    return {
        body: body,
    };
});


