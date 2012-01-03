if (!window.iContrib ) {
    iContrib = {};
}

iContrib.chartID = 'userChart';

iContrib.draw = function() {
	var chart = new google.visualization.PieChart(document.getElementById(iContrib.chartID));
	var data = new google.visualization.DataTable();
	var options = {title: "Greg's Distribution", width: 400, height: 300, backgroundColor: { fill:'transparent' }};
	data.addColumn('string', 'Charity');
	data.addColumn('number', 'Percentage');
	data.addRows([
		['UNICEF', 35],
		['American Red Cross', 10],
		['La Jolla Playhouse', 10],
		['San Diego Foundation', 10], 
		['USA UWH', 80],
		['LACC', 20],
	]);
	chart.draw(data, options);
};

iContrib.onLoad = function() {
	google.load('visualization', '1.0', {'packages':['corechart'], "callback" : iContrib.draw});
};

iContrib.drawChart = function(chartID) { 
	iContrib.chartID = chartID;

	iContrib.initLoader();
}

iContrib.initLoader = function() {
	var googleCharts = document.getElementById("googleCharts");

	if(googleCharts) {
		iContrib.draw();
	} else {
		var script = document.createElement("script");
		script.id = "googleCharts";
		script.src = "https://www.google.com/jsapi?callback=iContrib.onLoad";
		script.type = "text/javascript";
		document.getElementsByTagName("head")[0].appendChild(script); 
	}
};	