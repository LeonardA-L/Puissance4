(function(window, document) {

    /* Semi-global variables */
    var ws, board, boardElement;

    var BOARD_WIDTH = 7;
    var BOARD_HEIGHT = 6;

    var EMPTY_SYMBOL = '_';
    var RED_SYMBOL = 'r';
    var YELLOW_SYMBOL = 'y';


    var clearBoard = function() {
        createBoard();
        boardToTable();
    };

    var createBoard = function() {
        board = [];
        for (var i = 0; i < BOARD_HEIGHT; i++) {
            var row = [];
            for (var j = 0; j < BOARD_WIDTH; j++) {
                row.push(EMPTY_SYMBOL);
            }
            board.push(row);
        }
    };

    var setCellClassesTo = function(row, column, classes) {
        var elem = boardElement.querySelectorAll('tr')[row].querySelectorAll('td')[column];
        elem.setAttribute('class', classes);
    };

    var setRedAt = function(row, column) {
        setCellClassesTo(row, column, 'cell red');
    };

    var setYellowAt = function(row, column) {
        setCellClassesTo(row, column, 'cell yellow');
    };

    var setEmptyAt = function(row, column) {
        setCellClassesTo(row, column, 'cell');
    };

    var boardToTable = function() {
        if (!board) {
            board = createBoard();
        }

        for (var row = 0; row < BOARD_HEIGHT; row++) {
            for (var col = 0; col < BOARD_WIDTH; col++) {
                var sym = board[row][col];
                if (RED_SYMBOL == sym) {
                    setRedAt(row, col);
                } else if (YELLOW_SYMBOL == sym) {
                    setYellowAt(row, col);
                } else {
                    setEmptyAt(row, col);
                }
            }
        }
    };

    var addCanvasElement = function() {
        var canvas = document.createElement('canvas');
        ctx = canvas.getContext('2d');
        canvas.id = 'canv1';
        document.body.appendChild(canvas);
    };

    var removeLoadingMessage = function() {
        var p = document.querySelector('#loading');
        p.remove();
    };

    var setUpWebSocket = function() {
        ws = new WebSocket('ws://127.0.0.1:9000/ws');

        ws.onopen = function(event) {
            console.log('WebSocket connection up and running!');
            removeLoadingMessage();
        }
    };

    var main = function() {
        setUpWebSocket();
        boardElement = document.querySelector('#board');
        createBoard();
    }
    window.setRedAt = setRedAt;
    window.setYellowAt = setYellowAt;
    window.clearBoard = clearBoard;
    window.onload = main;

})(this, this.document)