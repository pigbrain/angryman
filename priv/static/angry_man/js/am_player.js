
var player = player || {};

(function (player) {

    player.positionX = 0;
    player.positionY = 0;
    player.directionX = 0;
    player.directionY = 0;


    var move = function(directionX, directionY) {
       //cm.reqMove(directionX, directionY, player.positionX, player.positionY);
    }

    player.move = move;

})(player);