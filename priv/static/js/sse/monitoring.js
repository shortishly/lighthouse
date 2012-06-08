$(document).ready(function() {
    $.eventsource({
	label: "monitoring",
	url: "/es/sse/monitoring",
	dataType: "json",
	
	open: function(data) {
	    console.log(data);
	},
	
	message: function(data) {
	    console.log(data);

	    for (object in data) {
		for(field in data[object]) {
		    $("#" + object + " ." + field).sparkline(data[object][field]);
		}
	    }
	}
    });
});
