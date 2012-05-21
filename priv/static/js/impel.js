$(document).ready(function() {
    $.eventsource({
	label: "monitoring",
	url: "/monitoring",
	dataType: "json",
	
	open: function(data) {
	    console.log(data);
	},
	
	message: function(data) {
	    for (object in data) {
		for(field in data[object]) {
		    $("#" + object + " ." + field).sparkline(data[object][field]);
		}
	    }
	}
    });
});

$(document).ready(function() {
    var i;
    var tickers = $(".ticker");

    var eventsource = function(ticker) {
	$.eventsource({
	    url: "/es/" + $("dt:contains('Path') + dd", ticker).html(),
	    message: function(data) {
		$("dt:contains('Data') + dd", ticker).text(data);
	    }
	});
    }

    for(i = 0; i < tickers.length; i += 1) {
	eventsource(tickers[i]);
    }
});
