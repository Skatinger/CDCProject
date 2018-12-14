

//================= websocket ==============================

var webSocket = new WebSocket('ws://localhost:8080/wb');

webSocket.onopen = function(event) {
	console.log("socket connection established");
	$(".cell:contains('not connected')").text("updating...");
};

webSocket.onmessage = function(event) {
	onMessage(event)
};

webSocket.onclose = function(event) {
	console.log("socket connection closed");
}

webSocket.onerror = function(event) {
	alert("A connection error occured. See console for more info.");
};

// ============= message handling =============================

function onMessage(event) {
	console.log("received update.");
	json = JSON.parse(event.data);
	update_table(json);
	$(".cell:contains('not connected')").remove();
}

// json format:   species:count
//never declare global variables lol
//var old_count = {};
function update_table(json) {
	$.each(json, function(k, v) {
		if($('.cell:contains(' + k + ')').length > 0) {
			$("div[data-title='" + k + "_count']").text(v);
			//trend = old_count[k] < v ? "down" : "up";
			//$("div[data-title='" + k + "_trend']").text(trend);
			//old_count[k] = $("div[data-title='" + k + "_count']").text();
		} else {
			appendRow(k, v);
		}
    });
}

function appendRow(species, count) {
	$('.table').append('<div class="row ' + species + '"><div class="cell" data-title="species">' + species +
							'</div><div class="cell" data-title="' + species + '_count">' + count
							 + '</div><div class="cell" data-title="' + species + '_trend">linear</div></div>');
}
