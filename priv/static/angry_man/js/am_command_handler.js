
var commandId = {
    LOGIN_C2S : '100000',
    LOGIN_S2C : '100001',
    MOVE_C2S : '100010',
    MOVE_S2C : '100011',
    STOP_C2S : '100012',
    STOP_S2C : '100013',
    PLAYER_SYNC_C2S : '400001',
    PLAYER_SYNC_S2C : '400002',
    FIELD_SYNC_C2S : '400003',
    FIELD_SYNC_S2C : '400004',
    HEARTBEAT_C2S : '500000',
    HEARTBEAT_S2C : '500001'
};

var resultCode = {
    SUCCESS : '200'
};

var commandHandler = commandHandler || {};

(function (commandHandler, commandQueue) {

    var commandTable = [];

    var init = function() {

    };

    var registerCommand = function(commandId, callback) {
        commandTable[commandId] = callback;
    };

    var putCommand = function(command) {
        commandQueue.enqueue(command);
    };

    var update = function() {
        while(!commandQueue.isEmpty()) {
            var item = commandQueue.dequeue();
            if (item == undefined) {
                continue;
            }

            if (item.result == resultCode.SUCCESS) {
                commandTable[item.commandId](item);
            } else {
                console.log(item);
            }
        }
    }

    commandHandler.init = init;
    commandHandler.registerCommand = registerCommand;
    commandHandler.putCommand = putCommand;
    commandHandler.update = update;

})(commandHandler, commandQueue);
