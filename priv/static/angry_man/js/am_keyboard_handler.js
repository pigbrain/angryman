
var Keyboard = {};
Keyboard.STATUS = 0;
Keyboard.LEFT = 37;
Keyboard.RIGHT = 39;
Keyboard.UP = 38;
Keyboard.DOWN = 40;

Keyboard._keys = {};

Keyboard.listenForEvents = function (keys) {
    window.addEventListener('keydown', this._onKeyDown.bind(this));
    window.addEventListener('keyup', this._onKeyUp.bind(this));

    keys.forEach(function (key) {
        this._keys[key] = false;
    }.bind(this));
}

Keyboard._onKeyDown = function (event) {
    var keyCode = event.keyCode;
    if (keyCode in this._keys) {
        event.preventDefault();
        this._keys[keyCode] = true;

        if (Keyboard.STATUS !== keyCode) {
            switch (keyCode) {
                case Keyboard.LEFT :
                    this.moveLeft();
                    break;
                case Keyboard.RIGHT :
                    this.moveRight();
                    break;
                case Keyboard.UP :
                    this.moveUp();
                    break;
                case Keyboard.DOWN :
                    this.moveDown();
                    break;
            }

            Keyboard.STATUS = keyCode;
        }
    }
};

Keyboard._onKeyUp = function (event) {
    var keyCode = event.keyCode;
    if (keyCode in this._keys) {
        event.preventDefault();
        this._keys[keyCode] = false;

        Keyboard.STATUS = 0;

        this.moveStop();
    }
};

Keyboard.moveLeft = function() {
};
Keyboard.moveRight = function() {
};
Keyboard.moveUp = function() {
};
Keyboard.moveDown = function() {
};
Keyboard.moveStop = function() {
};

Keyboard.isDown = function (keyCode) {
    if (!keyCode in this._keys) {
        throw new Error('Keycode ' + keyCode + ' is not being listened to');
    }
    return this._keys[keyCode];
};