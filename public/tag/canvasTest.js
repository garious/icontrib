var deps = [
    '/tag/tag.js'
];

function onReady(E) {

    var canvas = E.canvas({width: 150, height: 250});

    var ctx = canvas.getContext('2d');  

    ctx.font = "20pt Arial";
    var s = "Hello world";
    var sz = ctx.measureText(s);
    ctx.fillText(sz.width + 'px', 10, 50);
    ctx.fillText(s, 10, 80);

    ctx.moveTo(10, 82);
    ctx.lineTo(10 + sz.width, 82);
    ctx.stroke();
  
    ctx.fillStyle = "rgb(200,0,0)";  
    ctx.fillRect(20, 120, 55, 50);  
  
    ctx.fillStyle = "rgba(0, 0, 200, 0.5)";  
    ctx.fillRect(40, 140, 55, 50);

    define(canvas);
}

require(deps, onReady);

