class GrourdleClient extends EventTarget {
    #ws;
    #sessionId;

    constructor() {
        super();
        let host = location.host;
        this.#ws = new WebSocket("ws://" + host + "/ws");
        this.#ws.grourdle = this;
        this.#ws.onopen = this.#onWsOpen;
        this.#ws.onmessage = this.#onWsMessage;
        this.#addEventListeners();

        let ws = this.#ws;
        setInterval(function(){
            let date = new Date();
            ws.send(JSON.stringify({"keepalive" : date.getTime()}));
            }, 30000);
    }

    #addEventListeners() {
        this.addEventListener("session_bound", this.onSessionBound);
        this.addEventListener("sess_echo", (e) => {
            console.log("got sess_echo resp: " + e.data.sess_echo)
        });
    }

    wsSend(msg) {
        let text = JSON.stringify(msg);
        this.#ws.send(text);
    }

    // === Outgoing Messages ===

    setUsername(username) {
        this.wsSend({set_username: username});
    }

    createGame() {
        this.wsSend({join_game: "new"});
    }

    joinGame(gameId) {
        this.wsSend({join_game: gameId});
    }

    startGame() {
        this.wsSend({start_game: true});
    }

    submitGuess(guess) {
        this.wsSend({guess: guess});
    }

    // === Callbacks ===

    #onWsMessage(wsevent) {
        // console.log("websocket event: ");
        // console.log(wsevent); // todo: eventually remove unnecessary log statements

        if(!wsevent.data)
            return;
        try {
            let msg = JSON.parse(wsevent.data)
            let e = new Event(msg.event);
            e.data = msg;
            // console.log("dispatching event: ");
            console.log(msg);
            this.grourdle.dispatchEvent(e); // this = WebSocket
        } catch (e) {
            console.log("failed to parse wsevent data.")
        }
    }

    #onWsOpen() {
        console.log("established ws connection");

        this.grourdle.wsSend({start_session: true}); // this = WebSocket
    }

    onSessionBound(e) {
        this.#sessionId = e.data.sess_id;
        console.log("session bound: " + this.#sessionId);

        this.wsSend({sess_echo: "session handling works!"});
    }
}