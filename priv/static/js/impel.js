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
		$("#monitoring .processes").sparkline(data.monitor.processes);
		$("#monitoring .reductions").sparkline(data.monitor.reductions);
		$("#monitoring .input_bytes").sparkline(data.monitor.input_bytes);
		$("#monitoring .output_bytes").sparkline(data.monitor.output_bytes);
		$("#monitoring .run_queue").sparkline(data.monitor.run_queue);
		$("#monitoring .context_switches").sparkline(data.monitor.context_switches);
		console.log(data);
	    }
	});
    })();
})();