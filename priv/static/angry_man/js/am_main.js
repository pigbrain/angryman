

Game.load = function () {
    return [
        Loader.loadImage('tiles', '/static/angry_man/img/tiles.png'),
        Loader.loadImage('am1', '/static/angry_man/img/am1.png'),
        Loader.loadImage('am2', '/static/angry_man/img/am2.png')
    ];
};

Game.init = function () {
    Keyboard.listenForEvents(
        [Keyboard.LEFT, Keyboard.RIGHT, Keyboard.UP, Keyboard.DOWN]);
    this.tileAtlas = Loader.getImage('tiles');
    this.player = new Player(map, 160, 160, null);
    this.playerList = {  };
    this.camera = new Camera(map, 1024, 768);
    this.camera.follow(this.player);
};

Game.update = function (delta) {
    this.player.move(delta);
    for(var p in this.playerList) {
        this.playerList[p].move(delta);
    }

    this.camera.update();

    commandHandler.update();
};

Game._drawLayer = function (layer) {
    var startCol = Math.floor(this.camera.x / map.tsize);
    var endCol = startCol + (this.camera.width / map.tsize);
    var startRow = Math.floor(this.camera.y / map.tsize);
    var endRow = startRow + (this.camera.height / map.tsize);
    var offsetX = -this.camera.x + startCol * map.tsize;
    var offsetY = -this.camera.y + startRow * map.tsize;

    for (var c = startCol; c <= endCol; c++) {
        for (var r = startRow; r <= endRow; r++) {
            var tile = map.getTile(layer, c, r);
            var x = (c - startCol) * map.tsize + offsetX;
            var y = (r - startRow) * map.tsize + offsetY;
            if (tile !== 0) { // 0 => empty tile
                this.ctx.drawImage(
                    this.tileAtlas,
                    (tile - 1) * map.tsize,
                    0,
                    map.tsize,
                    map.tsize,
                    Math.round(x),
                    Math.round(y),
                    map.tsize,
                    map.tsize
                );
            }
        }
    }
};

Game.render = function () {

    if (!this.player.image) {
        return;
    }

    this._drawLayer(0);

    this.ctx.drawImage(
        this.player.image,
        this.player.screenX - this.player.width / 2,
        this.player.screenY - this.player.height / 2);

    for(var p in this.playerList) {

        var x = this.player.screenX - (this.player.getX() - this.playerList[p].getX()) - (this.player.width / 2);
        if (x < 10 || x > 1020) {
            continue;
        }

        var y = this.player.screenY - (this.player.getY() - this.playerList[p].getY()) - this.player.height / 2;
        if (y < 10 || y > 750) {
            continue;
        }

        this.ctx.drawImage(
            this.playerList[p].image,
            x,
            y);
    }

    this._drawLayer(1);
};

window.onload = function () {

    commandHandler.init();
    ws.init();

    commandHandler.registerCommand(commandId.HEARTBEAT_S2C, resHeartBeat);
    commandHandler.registerCommand(commandId.LOGIN_S2C, resLogin);
    commandHandler.registerCommand(commandId.MOVE_S2C, resMove);
    commandHandler.registerCommand(commandId.STOP_S2C, resStop);
    commandHandler.registerCommand(commandId.PLAYER_SYNC_S2C, resPlayerSync);
    commandHandler.registerCommand(commandId.FIELD_SYNC_S2C, resFieldSync);

    var context = document.getElementById('field').getContext('2d');
    Game.run(context);
};

var resHeartBeat = function(data) { };

var resLogin = function(data) {

    Game.player.setId(data.playerId);
    Game.player.setX(data.positionX);
    Game.player.setY(data.positionY);
    Game.player.setImage(data.character == 'am1' ? Loader.getImage('am1') : Loader.getImage('am2'));

    var command = new Object();
    command.commandId = commandId.FIELD_SYNC_C2S;
    ws.post(command);

};

var resMove = function(data) {
    Game.player.setX(data.positionX);
    Game.player.setY(data.positionY);
};

var resStop = function(data) {
    Game.player.setX(data.positionX);
    Game.player.setY(data.positionY);
};

var resPlayerSync = function (data) {
    if (data.state == 'login') {
        Game.playerList[data.playerId] = new Player(map, data.positionX, data.positionY, data.character == 'am1' ? Loader.getImage('am1') : Loader.getImage('am2'));
        return;
    }

    player = Game.playerList[data.playerId];
    if (player == undefined) {
        return;
    }

    player.setX(data.positionX);
    player.setY(data.positionY);
    player.setDirX(data.directionX);
    player.setDirY(data.directionY);
    player.setState(data.state);
}

var resFieldSync = function(data) {

    var playerList = data.playerList;
    for (index in playerList) {
        var player = JSON.parse(playerList[index]);
        if (Game.player.getId() == player.playerId) {
            continue;
        }

        Game.playerList[player.playerId] = new Player(map, player.positionX, player.positionY, player.character == 'am1' ? Loader.getImage('am1') : Loader.getImage('am2'));
        Game.playerList[player.playerId].setDirX(player.directionX);
        Game.playerList[player.playerId].setDirY(player.directionY);
        Game.playerList[player.playerId].setState(player.state);
    }

    $("#login").hide();
    $("#play").show();
};

Keyboard.moveLeft = function() {
    Game.player.setDirX(-1);
    Game.player.setDirY(0);
    Game.player.setState('move');

    var command = new Object();
    command.commandId = commandId.MOVE_C2S;
    command.directionX = -1;
    command.directionY = 0;
    command.positionX = Game.player.getX();
    command.positionY = Game.player.getY();

    ws.post(command);
};

Keyboard.moveRight = function() {

    Game.player.setDirX(1);
    Game.player.setDirY(0);
    Game.player.setState('move');

    var command = new Object();
    command.commandId = commandId.MOVE_C2S;
    command.directionX = 1;
    command.directionY = 0;
    command.positionX = Game.player.getX();
    command.positionY = Game.player.getY();

    ws.post(command);
};

Keyboard.moveUp = function() {

    Game.player.setDirX(0);
    Game.player.setDirY(-1);
    Game.player.setState('move');

    var command = new Object();
    command.commandId = commandId.MOVE_C2S;
    command.directionX = 0;
    command.directionY = -1;
    command.positionX = Game.player.getX();
    command.positionY = Game.player.getY();

    ws.post(command);
};

Keyboard.moveDown = function() {

    Game.player.setDirX(0);
    Game.player.setDirY(1);
    Game.player.setState('move');

    var command = new Object();
    command.commandId = commandId.MOVE_C2S;
    command.directionX = 0;
    command.directionY = 1;
    command.positionX = Game.player.getX();
    command.positionY = Game.player.getY();
    ws.post(command);
};

Keyboard.moveStop = function() {

    Game.player.setState('stop');

    var command = new Object();
    command.commandId = commandId.STOP_C2S;
    command.directionX = 0;
    command.directionY = 0;
    command.positionX = Game.player.getX();
    command.positionY = Game.player.getY();

    ws.post(command);
};