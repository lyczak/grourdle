class GrourdleGame {
    // game states
    LOBBY = "lobby";
    WAITING = "waiting";
    ACTIVE = "active";

    // cell colors
    GREEN = "#00af00";
    YELLOW = "#d4b800";
    BLACK = "#484848";

    // properties
    client;
    state;

    constructor() {
        this.state = this.LOBBY;
    }

    ready() {
        let client = this.client;
        document.querySelector("#new-game-button")
            .addEventListener("click", (e) => {
                client
            });
        this.client = new GrourdleClient();
        this.#addEventListeners();
    }

    #addEventListeners() {
        this.client.addEventListener("game_joined", this.onGameJoined);
        this.client.addEventListener("game_left", this.onGameLeft);
        this.client.addEventListener("game_start", this.onGameStart);
        this.client.addEventListener("guess_submitted", this.onGuessSubmitted);
        this.client.addEventListener("round_ended", this.onRoundEnd);
        this.client.addEventListener("game_ended", this.onGameEnd);
    }

    // === Callbacks ===

    // A single user joins the game we are currently in (not ourself).
    // { "event": "game_joined", "user_count": numberOfUsers }
    onGameJoined(e) {
        console.log("a new user joined the game. there are now: " + e.data.user_count);
    }

    // A single user leaves the game we are currently in (not ourself).
    // { "event": "game_left", "user_count": numberOfUsers }
    onGameLeft(e) {
        console.log("a user left the game. there are now: " + e.data.user_count);
    }

    // The game transitions from the waiting state to the active state
    // TODO: add erlang broadcast for this event
    onGameStart(e) {
        console.log("the game has started!");
    }

    // A user has submitted a guess
    // { "event": "guess_submitted", "guess_count": numberOfGuesses }
    // TODO: update erlang code to fix typo ^
    onGuessSubmitted(e) {
        console.log("another guess has been submitted. there are now: " + e.data.guess_count);
    }

    // the round has ended and a guess has been chosen and the board has been updated
    // { "event": "round_ended", "guess_chosen": theGuess, "guesses": ["guess", "guess"], "result": theResult }
    onRoundEnd(e) {
        console.log("the round has ended!");
        console.log("here are the guesses: " + e.data.guesses);
        console.log("winning guess: " + e.data.guess_chosen);
        console.log("round result: " + e.data.result);
    }

    // the game transitions from the active state to the waiting state
    // { "event": "game_ended", "board": [result1, result2] }
    onGameEnd(e) {
        console.log("the game has ended! here is the board: " + e.data.board);
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
}