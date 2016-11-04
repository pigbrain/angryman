
var cm = cm || {};

(function (cm, ws) {
    var commandId = {
        LOGIN_C2S : '100000',
        LOGIN_S2C : '100001',
        MOVE_C2S : '100010',
        MOVE_S2C : '100011',

        HEARTBEAT_C2S : '500000',
        HEARTBEAT_S2C : '500001'
    };

    var init = function() {
        ws.registerCommand(commandId.HEARTBEAT_S2C, resHeartBeat);
        ws.registerCommand(commandId.LOGIN_S2C, resLogin);
        ws.registerCommand(commandId.MOVE_S2C, resMove);
    };

    var resHeartBeat = function(data) { };

    var reqLogin = function(nickname, character) {

        var command = new Object();
        command.commandId = commandId.LOGIN_C2S;
        command.character = character;
        command.nickname = nickname == '' ? Math.random().toString(36).substring(5) : nickname;

        post(command);
    };

    var resLogin = function(data) {
        console.log("response login " + data);

        $("#login").hide();
        $("#play").show();
    };

    var reqMove = function(directionX, directionY, positionX, positionY) {

        var command = new Object();
        command.commandId = commandId.MOVE_C2S;
        command.directionX = directionX;
        command.directionY = directionY;
        command.positionX = positionX;
        command.positionY = positionY;

        post(command);
    };

    var resMove = function(data) {
        console.log("response move " + data);

    };

    var post = function(command) {
        ws.send(JSON.stringify(command));
    };

    cm.init = init;
    cm.reqLogin = reqLogin;
    cm.reqMove = reqMove;

})(cm, ws);
