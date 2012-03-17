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
		$("#monitoring .processes").html(data.monitor.processes);
		$("#monitoring .reductions").html(data.monitor.reductions);
		$("#monitoring .input_bytes").html(data.monitor.input_bytes);
		$("#monitoring .output_bytes").html(data.monitor.output_bytes);
		$("#monitoring .run_queue").html(data.monitor.run_queue);
		$("#monitoring .context_switches").html(data.monitor.context_switches);
		console.log(data);
	    }
	});
    })();
})();