var canvas = document.createElement('canvas');

if (!canvas.getContext){
    alert('Sorry, but to see this demo, you need a web browser that supports the "canvas" element.');
}

canvas.height = 1000;
canvas.width  = 1000;

var ctx = canvas.getContext('2d');

for (var i = 0; i < 4; i++) {
    for (var j = 0; j < 3; j++) {
        ctx.beginPath();
        var x          = 100 + j * 100;               // x coordinate
        var y          = 100 + i * 100;               // y coordinate
        var radius     = 40;                          // Arc radius
        var startAngle = 0;                           // Starting point on circle
        var endAngle   = Math.PI + (Math.PI * j) / 2; // End point on circle
        var clockwise  = i % 2 === 0 ? false : true;  // clockwise or counterclockwise

        ctx.arc(x, y, radius, startAngle, endAngle, clockwise);

        if (i > 1){
          ctx.fill();
        } else {
          ctx.stroke();
        }
    }
}

Yoink.define( canvas );

