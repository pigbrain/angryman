

var ws = ws || {};

(function (ws, commandHandler) {

    var webSocket = undefined;

    var init = function () {
        webSocket = new WebSocket("ws://" + window.location.hostname + ":" + window.location.port + "/am");
        webSocket.onopen = function (evt) {
            onOpen(evt);
        };
        webSocket.onclose = function (evt) {
            onClose(evt);
        };
        webSocket.onmessage = function (evt) {
            onMessage(evt);
        };
        webSocket.onerror = function (evt) {
            onError(evt);
        };
    };

    var destroy = function() {
        webSocket.close();
    };

    var onOpen = function(evt) {
    };

    var onClose = function(evt) {
    };

    var onMessage = function(evt) {
        commandHandler.putCommand(JSON.parse(evt.data));
    };

    var onError = function(evt) {
    };

    var post = function(data) {
        if(webSocket.readyState == webSocket.OPEN) {
            webSocket.send(JSON.stringify(data));
        }
    };

    ws.init = init;
    ws.destroy = destroy;
    ws.post = post;
})(ws, commandHandler);
