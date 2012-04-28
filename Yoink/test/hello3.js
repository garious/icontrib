function onReady(Tag) {
    Yoink.define( Tag.tag('p', 'Hello World') );
}

Yoink.require(['/Tag/Tag.js'], onReady);

