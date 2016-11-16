
var commandQueue = commandQueue || {};

(function (commandQueue) {
    commandQueue.queue = [];
    commandQueue.offset = 0;

    commandQueue.getLength = function () {
        return (commandQueue.queue.length - commandQueue.offset);
    };

    commandQueue.isEmpty = function () {
        return (commandQueue.queue.length == 0);
    };

    commandQueue.enqueue = function (item) {
        commandQueue.queue.push(item);
    };

    commandQueue.dequeue = function () {

        if (commandQueue.queue.length == 0) {
            return undefined;
        }

        var item = commandQueue.queue[commandQueue.offset];

        if (++commandQueue.offset * 2 >= commandQueue.queue.length) {
            commandQueue.queue = commandQueue.queue.slice(commandQueue.offset);
            commandQueue.offset = 0;
        }

        return item;
    };

    commandQueue.peek = function () {
        return (commandQueue.queue.length > 0 ? commandQueue.queue[commandQueue.offset] : undefined);
    };
})(commandQueue);
