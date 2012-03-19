Impel = (function() {
    (function() {
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
    })();
})();