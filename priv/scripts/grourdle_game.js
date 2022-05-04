class GrourdleGame {
    // game states
    LOBBY = "lobby";
    WAITING = "waiting";
    ACTIVE = "active";

    // cell colors
    GREEN = "#008000";
    YELLOW = "#c2a200";
    BLACK = "#484848";
    BLUE = "#27397b"

    // properties
    isOwner = false;
    client;
    state;
    userCount = 1;
    board = [];
    guesses = [];

    guess = "";
    cellIndex = 0;

    constructor() {
        this.state = this.LOBBY;
    }

    ready() {
        this.client = new GrourdleClient();
        this.client.game = this;
        this.#addEventListeners();

        let client = this.client;
        let welcomeContainer = document.querySelector("#welcome-container");
        document.querySelector("#new-game-button")
            .addEventListener("click", (e) => {
                welcomeContainer.style.display = "none";
                document.getElementById("start-game-button").style.display = "block";
                client.createGame();
                client.game.isOwner = true;
            });
        document.querySelector("#join-game-button")
            .addEventListener("click", (e) => {
                welcomeContainer.style.display = "none";
                let gid = prompt("Please enter the game-id:");
                client.joinGame(gid);
            });
        document.getElementById("start-game-button")
            .addEventListener("click", (e) => {
                document.getElementById("start-game-button").style.display = "none";
                client.startGame();
            });

        let game = this;
        game.setupBoard();
        document.addEventListener("keydown", (e) => {
            if (game.state !== "active") return;

            const cell = document.getElementById("cell" + game.cellIndex);
            const key = e.key;

            if (key === "Enter") {
                if (game.guess.length < 5) return;
                game.setRowWaiting(game.guesses.length);

                client.submitGuess(game.guess);

                // game.cellIndex += 1; // next row
                game.guess = "";
            }

            if (key === "Backspace") {
                if (game.guess.length <= 0) return;

                game.cellIndex -= 1;
                document.getElementById("cell" + game.cellIndex).textContent = "";
                game.guess = game.guess.slice(0, -1);
                // bulge(document.getElementById(nextId), 0.8);

                return;
            }

            if (!(key.length === 1 && key.match(/[a-z]/i))) return;

            if (game.guess.length < 5) {
                game.guess += key;
                cell.textContent = key.toUpperCase();
                game.cellIndex += 1;
            }
        });
    }

    #addEventListeners() {
        this.client.addEventListener("game_state_updated", this.onStateUpdated);
        this.client.addEventListener("game_bound", this.onGameBound);
        this.client.addEventListener("game_unbound", this.onGameUnbound);
        this.client.addEventListener("game_joined", this.onGameJoined);
        this.client.addEventListener("game_left", this.onGameLeft);
        this.client.addEventListener("game_started", this.onGameStart);
        this.client.addEventListener("guess_submitted", this.onGuessSubmitted);
        this.client.addEventListener("round_ended", this.onRoundEnd);
        this.client.addEventListener("game_ended", this.onGameEnd);
    }

    // === Callbacks ===

    // general catchall game state out of sync
    //     event => game_state_updated,
    //     game_state => GState,
    //     board => Board,
    //     board_guesses => AllTheGuesses,
    //     user_count => NumberOfUsers,
    //     guess_count => NumberOfRoundGuesses
    onStateUpdated(e) {
        let g = this.game;
        let d = e.data;
        g.state = d.game_state;
        g.board = d.board;
        g.guesses = d.board_guesses;
        g.userCount = d.user_count;
        g.cellIndex = g.guesses.length * 5;

        g.clearBoard();
        for (let i = 0; i < g.guesses.length; i++) {
            g.setRow(i, g.guesses[i]);
            g.setRowColors(i, g.board[i]);
        }

        if (g.state === g.WAITING) {
            g.setStatus("Waiting to Start");
        } else {
            document.getElementById("board-container").style.display = "";
            g.setGuessCount(d.guess_count);
        }
    }

    // We have successfully joined a game.
    // { "event": "game_bound", "game_id": theId }
    onGameBound(e) {
        console.log("we have now joined game: " + e.data.game_id);
        document.querySelector("#room-id").textContent = e.data.game_id
        document.querySelector("#game-container").style.display = "";
        // this.game.setStatus("Waiting to Start");
    }

    // We have lost our game!
    // { "event": "game_unbound", "reason": someReason }
    onGameUnbound(e) {
        console.log("our game has come unbound for the following reason: " + e.data.reason);
        alert("This current room has closed!");
        window.location.reload(); // easiest thing to do at this point is just restart, sending user back to lobby
    }

    // A single user joins the game we are currently in (could also have been us).
    // { "event": "game_joined", "user_count": numberOfUsers }
    onGameJoined(e) {
        console.log("a new user joined the game. there are now: " + e.data.user_count);
        this.game.userCount = e.data.user_count;
    }

    // A single user leaves the game we are currently in (could also have been us).
    // { "event": "game_left", "user_count": numberOfUsers }
    onGameLeft(e) {
        console.log("a user left the game. there are now: " + e.data.user_count);
        this.game.userCount = e.data.user_count;
    }

    // The game transitions from the waiting state to the active state
    onGameStart(e) {
        console.log("the game has started!");
        document.getElementById("board-container").style.display = "";
        this.game.resetLocalState();
        this.game.clearBoard();
        this.game.state = this.game.ACTIVE;
        this.game.setGuessCount(0);
    }

    // A user has submitted a guess
    // { "event": "guess_submitted", "guess_count": numberOfGuesses }
    onGuessSubmitted(e) {
        console.log("another guess has been submitted. there are now: " + e.data.guess_count);
        this.game.setGuessCount(e.data.guess_count);
        // todo: show the guess disappearing or being grey/pulsing
    }

    // the round has ended and a guess has been chosen and the board has been updated
    // { "event": "round_ended", "guess_chosen": theGuess, "guesses": ["guess", "guess"], "result": theResult }
    onRoundEnd(e) {
        let g = this.game;
        let d = e.data;
        console.log("the round has ended!");
        console.log("here are the guesses: " + d.guesses);
        console.log("winning guess: " + d.guess_chosen);
        console.log("round result: " + d.result);

        let row = g.guesses.length;
        g.setRow(row, d.guess_chosen);
        g.setRowColors(row, d.result);
        g.setGuessCount(0);

        g.guesses.push(d.guess_chosen);
        g.board.push(d.result);
    }

    // the game transitions from the active state to the waiting state
    // { "event": "game_ended", "board": [result1, result2], "reason": "won"/"lost" }
    onGameEnd(e) {
        console.log("the game has ended! here is the board: " + e.data.board);
        this.game.state = this.WAITING;
        switch (e.data.reason) {
            case "won":
                this.game.setStatus("You won!");
                break;
            case "lost":
                this.game.setStatus("You lost!");
                break;
            default:
                this.game.setStatus("Game over!");
        }

        if (this.game.isOwner) {
            document.getElementById("start-game-button").innerText = "Play Again";
            document.getElementById("start-game-button").style.display = "block";
        }
    }

    // === Utility ===

    setupBoard() {
        let bc = document.querySelector("#board-container");
        for (let i = 0; i < 30; i++) {
            let cell = document.createElement("div");
            cell.setAttribute("class", "cell");
            cell.setAttribute("id", "cell" + i);
            bc.appendChild(cell);
        }
    }

    setStatus(s) {
        document.getElementById("status-header")
            .textContent = s;
    }

    // g guesses out of p players
    setGuessCount(g) {
        this.setStatus(g + "/" + this.userCount + " Guesses");
    }

    getCell(i) {
        return document.getElementById("cell" + i);
    }

    resetLocalState() {
        this.cellIndex = 0;
        this.guess = "";
        this.guesses = [];
        this.board = [];
    }

    // clear board and reset
    clearBoard() {
        for (let i = 0; i < 6; i++) {
            this.clearRow(i);
        }
    }

    foreachInRow(row, fun) {
        let b = row * 5;
        for (let i = 0; i < 5; i++) {
            fun(this.getCell(b + i), i);
        }
    }

    clearRow(row) {
        this.foreachInRow(row, (r, i) => {
            r.textContent = "";
            r.style.backgroundColor = "";
        })
    }

    setRowColors(row, colors) {
        this.foreachInRow(row, (r, i) =>
            r.style.backgroundColor = this.parseColor(colors[i]));
    }

    setRowWaiting(row) {
        this.foreachInRow(row, (r, i) => r.style.backgroundColor = this.BLUE);
    }

    setRow(row, guess) {
        this.foreachInRow(row, (r, i) =>
            r.textContent = guess.slice(i, i + 1).toUpperCase());
    }

    parseColor(c) {
        switch (c) {
            case "green":
                return this.GREEN;
            case "yellow":
                return this.YELLOW;
            default:
                return this.BLACK;
        }
    }
}

let game = new GrourdleGame();
game.ready();