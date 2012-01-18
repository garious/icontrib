$(document).ready(function() {
	$('#navBar').jqDock( { align: 'bottom', labels: 'tl', duration: 150, step: 25, distance: 90, fadein: 300 } );
	
	var $body = $('#body');
	$("#navBar a").click(function() {
		var $link = $(this);
		if($body.length === 1) { 
			$body.load($link.prop("href"));
			return false;
		}
	});
});
