function Player(map, x, y, image) {
    this.map = map;
    this.id = undefined;
    this.x = x;
    this.y = y;
    this.dirX = 0;
    this.dirY = 0;
    this.width = map.tsize;
    this.height = map.tsize;
    this.image = image;
    this.state = undefined;
}

Player.SPEED = 256; // pixels per second

Player.prototype.setId = function(id) {
    this.id = id;
};
Player.prototype.setY = function(y) {
    this.y = y;
};
Player.prototype.setX = function(x) {
    this.x = x;
};
Player.prototype.setDirY = function(dirY) {
    this.dirY = dirY;
};
Player.prototype.setDirX = function(dirX) {
    this.dirX = dirX;
};
Player.prototype.setState = function(state) {
    this.state = state;
};
Player.prototype.setImage = function(image) {
    this.image = image;
};
Player.prototype.getId = function() {
    return this.id;
};
Player.prototype.getX = function() {
    return this.x;
};
Player.prototype.getY = function() {
    return this.y;
};
Player.prototype.getDirX = function() {
    return this.dirX;
};
Player.prototype.getDirY = function() {
    return this.dirY;
};
Player.prototype.getState = function() {
    return this.state;
};
Player.prototype.move = function (delta) {
    if (this.state != "move") {
        return;
    }

    delta = Math.floor(delta * 100) / 100;

    // move hero
    this.x += Math.floor(this.dirX * Player.SPEED * delta * 1000) / 1000;
    this.y += Math.floor(this.dirY * Player.SPEED * delta * 1000) / 1000;

    // check if we walked into a non-walkable tile
    this._collide();

    // clamp values
    var maxX = this.map.cols * this.map.tsize;
    var maxY = this.map.rows * this.map.tsize;
    this.x = Math.max(0, Math.min(this.x, maxX));
    this.y = Math.max(0, Math.min(this.y, maxY));
};

Player.prototype._collide = function () {
    var row, col;
    // -1 in right and bottom is because image ranges from 0..63
    // and not up to 64
    var left = this.x - this.width / 2;
    var right = this.x + this.width / 2 - 1;
    var top = this.y - this.height / 2;
    var bottom = this.y + this.height / 2 - 1;

    // check for collisions on sprite sides
    var collision =
        this.map.isSolidTileAtXY(left, top) ||
        this.map.isSolidTileAtXY(right, top) ||
        this.map.isSolidTileAtXY(right, bottom) ||
        this.map.isSolidTileAtXY(left, bottom);
    if (!collision) {
        return;
    }

    if (this.dirY > 0) {
        row = this.map.getRow(bottom);
        this.y = -this.height / 2 + this.map.getY(row);
    } else if (this.dirY < 0) {
        row = this.map.getRow(top);
        this.y = this.height / 2 + this.map.getY(row + 1);
    } else if (this.dirX > 0) {
        col = this.map.getCol(right);
        this.x = -this.width / 2 + this.map.getX(col);
    } else if (this.dirX < 0) {
        col = this.map.getCol(left);
        this.x = this.width / 2 + this.map.getX(col + 1);
    }
};

