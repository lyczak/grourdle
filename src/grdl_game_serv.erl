-module(grdl_game_serv).
-behaviour(gen_server).

-export([start_link/0, bind_to_owner/1, join_game/1, leave_game/2,
  start_game/1, submit_guess/2, end_round/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(BOARD_SIZE, 6).
-record(state, {
  owner, % owner's session pid for easy access
  sess, % #{Pid1 => Monitor1 ... PidN => MonitorN}
  word, % the game has a secret word, doesnt it?
  game_state, % waiting, active,
  board, % [[Color1, Color2 ... Color5] ... [Color1, Color2 ... Color5]]
  round_guesses, % #{Pid1 => Guess1 ... PidN => GuessN}
  board_guesses % [Guess1, Guess2 ... Guess6]
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc start and link to a new game server.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% @doc make the calling session process the owner of this game server.
%% there are certain actions that only the owner can perform.
%% @param takes the PID of the server linked above.
%% When the request is received, the gen_server calls handle_call({bind_to_session}...).
bind_to_owner(GsPid) ->
  gen_server:call(GsPid, {bind_to_session}).

%% @doc join the calling session process into this game server.
%% do not call this from the owner session, instead use bind_to_owner.
%% When the request is received, the gen_server calls handle_call({join_game}...).
join_game(GsPid) ->
  gen_server:call(GsPid, {join_game}).

%% @doc remove the calling session process from this game server.
%% any session can call this but if the owner leaves, it will terminate the game server.
%% @param takes the PID of the server linked above and a Reason for leaving the game.
%% When the request is received, the gen_server calls handle_call({leave_game}...).
leave_game(GsPid, Reason) ->
  gen_server:call(GsPid, {leave_game, Reason}).

%% @doc transition from the waiting state to start a new game.
%% only the owner session can call this.
%% @param takes the PID of the server linked above.
%% When the request is received, the gen_server calls handle_call({start_game}...).
start_game(GsPid) ->
  gen_server:call(GsPid, {start_game}).

%% @doc submit a guess on behalf of the calling session process.
%% existing guesses will not be changed.
%% this should only be called during an active round.
%% @param takes the PID of the server linked above and the Guess.
%% When the request is received, the gen_server calls handle_call({submit_guess, Guess}...).
submit_guess(GsPid, Guess) ->
  gen_server:call(GsPid, {submit_guess, Guess}).

%% @doc end the current round by choosing a guess and evaluating it
%% against the secret word. this should only be called during an active round.
%% @param takes the PID of the server linked above.
%% When the request is received, the gen_server calls handle_call({end_round}...).
end_round(GsPid) ->
  gen_server:call(GsPid, {end_round}).

%%%===================================================================
%%% gen_server
%%%===================================================================

%% @doc initialize the state of a new game server.
%% @param list of variables needed to initialization (if needed).
%% @returns tuple of initialized variables for the game.
init([]) ->
  {ok, #state{
    sess = #{},
    game_state = waiting,
    board = [],
    board_guesses = [],
    round_guesses = #{}}}.

%% == calls ==

%% @see bind_to_owner/1.
%% @doc create a new monitor and add it to the sess refs. add owner pid to state.
%% @param message {bind_to_session}, {SsPid, _Tag}, and state S = #state{sess = Refs}.
%% @returns a tuple {reply,Reply,State} -> {reply, ok, S#state...}.
handle_call({bind_to_session}, {SsPid, _Tag}, S = #state{sess = Refs}) ->
  Ref = erlang:monitor(process, SsPid),
  {reply, ok, S#state{
    owner = SsPid,
    sess = Refs#{SsPid => Ref}
  }};

%% @see join_game/1.
%% @doc create a new monitor and add it to the sess refs.
%% tells all users a user has joined.
%% @param message {join_game}, {Pid, _Tag}, and state S1 = #state{sess = Refs}.
%% @returns a tuple {reply,Reply,State} -> {reply, ok, S2}.
handle_call({join_game}, {Pid, _Tag}, S1 = #state{sess = Refs}) ->
  Ref = erlang:monitor(process, Pid),
  UserCount = maps:size(Refs) + 1,
  S2 = S1#state{
    sess = Refs#{Pid => Ref}
  },
  broadcast(#{event => game_joined, user_count => UserCount}, S2),
  send_game_state(Pid, S2),
  {reply, ok, S2};

%% @see leave_game/1.
%% @doc remove existing monitor and sess pid entry.
%% tells all users a user has left.
%% @param message {leave_game}, {Pid, _Tag}, and state S = #state{sess = Refs}.
%% @returns a tuple {reply,Reply,State} -> {reply, ok, S#state...}.
handle_call({leave_game}, {Pid, _Tag}, S = #state{sess = Refs}) ->
  Ref = maps:get(Pid, Refs),
  erlang:demonitor(Ref),
  UserCount = maps:size(Refs) - 1,
  broadcast(#{event => game_left, user_count => UserCount}, S),
  {reply, ok, S#state{
    sess = maps:remove(Ref, Refs)
  }};

%% @see start_game/1.
%% @doc only run when called by owner and game state is waiting.
%% set game_state to active, clear board and round guesses.
%% @param message {start_game}, {Pid, _Tag}, and state S = #state{owner = Owner, game_state = waiting}).
%% @returns a tuple {reply,Reply,State} -> {reply, ok, S#state...}.
handle_call({start_game}, {Pid, _Tag},
    S = #state{owner = Owner, game_state = waiting})
  when Pid == Owner orelse Pid == self() ->
  Word = grdl_wordle:get_answer(),
  broadcast(#{event => game_started}, S),
  {reply, ok, S#state{
    game_state = active,
    word = Word,
    board = [],
    board_guesses = [],
    round_guesses = #{}
  }};

%% @see submit_guess/2.
%% @doc if session's guess not exists, add it.
%% tells all users a guess has been submitted.
%% @param message {submit_guess, Guess}, {Pid, _Tag}, and state S = #state{sess = Sess, round_guesses = Round}).
%% @returns a tuple {reply,Reply,State} -> {reply, ok, S#state...}.
handle_call({submit_guess, Guess}, {Pid, _Tag}, S = #state{sess = Sess, round_guesses = Round}) ->
  case maps:find(Pid, Round) of
    {ok, _} ->
        {reply, ok, S};
    error ->
      GuessCount = maps:size(Round) + 1,
      broadcast(#{event => guess_submitted, guess_count => GuessCount}, S),
      case maps:size(Sess) of
        GuessCount -> self() ! {end_round}; %end_round(self());
        _ -> ok
      end,
      {reply, ok, S#state{
      round_guesses = Round#{Pid => Guess}
    }}
  end;

%% @doc handles all other calls.
%% @param message _Request,_From and state State = #state{}.
%% @returns State = #state{}.
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @doc handles all casts - asynchronous requests.
%% @param message _Request and state State = #state{}.
%% @returns {noreply, State}.
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @see end_round/1.
%% @doc handle everything needed for ending a round.
%% for both wins and losses.
%% @param message {end_round} that signals the end of the round.
%% @returns a tuple {noreply, State1} -> {noreply, S#state...}.
handle_info({end_round},
    S = #state{word = Word, board = Board, board_guesses = BoardGuesses, round_guesses = Round}) ->
  Guesses = maps:values(Round),
  case Guesses of
    [] -> self() ! {end_game, no_guesses}, {reply, ok, S};
    _ ->
      GuessChosen = grdl_guess_arbiter:choose_guess(Guesses),
      Result = grdl_wordle:check_guess(GuessChosen, Word),
      broadcast(#{
        event => round_ended,
        guess_chosen => GuessChosen,
        guesses => Guesses,
        result => Result}, S),
      case Result of
        [green,green,green,green,green] -> self() ! {end_game, won};
        _ when length(Board) >= ?BOARD_SIZE - 1 -> self() ! {end_game, lost};
        _ -> ok
      end,
      {noreply, S#state{
        round_guesses = #{},
        board = Board ++ [Result],
        board_guesses = BoardGuesses ++ [GuessChosen]
      }}
  end;

%% @doc ends the game by setting game_state to waiting, informs all users.
%% @param a message {end_game, Reason} that includes the reason for ending.
%% @returns a tuple {noreply, State1} -> {noreply, S#state...}.
handle_info({end_game, Reason},
    S = #state{board = Board, game_state = active}) ->
  io:format("game_serv ending game for reason ~p~n", [Reason]),
  broadcast(#{event => game_ended, reason => Reason, board => Board}, S),
  {noreply, S#state{
    game_state = waiting
  }};

%% @doc handles when the owner of the game leaves.
%% @param a message {'DOWN', _Ref, process, Pid, _}, and a state S = #state{owner = Owner, sess = Sess}.
%% @returns a tuple {noreply, State} -> {noreply, NewState} or {noreply, S}.
handle_info({'DOWN', _Ref, process, Pid, _}, S = #state{owner = Owner, sess = Sess}) ->
  case Pid of
    Owner ->
      io:format("i'm a game_serv and my owner just left~n"),
      broadcast(#{event => game_unbound, reason => owner_left}, S),
      exit(normal),
      {noreply, S};
    _ ->
      case maps:is_key(Pid, Sess) of
        true ->
          {reply, ok, NewState} = handle_call({leave_game}, {Pid, tag}, S),
          {noreply, NewState};
        false -> {noreply, S}
      end
  end;

%% @doc catchall to handle other info messages.
%% @param takes a message and a state _Info, State = #state{}.
%% returns {noreply, State}.
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @doc terminate function.
%% @param takes a reason as a message and a state: _Reason, _State = #state{}.
%% @returns ok.
terminate(_Reason, _State = #state{}) ->
  ok.

%% @doc handling code changes.
%% @param takes ethe older version as a message, a state, and anything extra.
%% @returns a tuple with a state {ok, State}.
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc helper func that given state and message, messages all session servers.
%% @param a message to broadcast and a state: Msg, _S = #state{sess = Sess}.
%% @returns ok.
broadcast(Msg, _S = #state{sess = Sess}) ->
  maps:foreach(fun(Pid,_) -> grdl_sess_serv:send_message(Pid, Msg) end, Sess),
  ok.

%% @doc sends some information about the current game state to a given session.
%% used when the user's session is presumed to be out-of-sync with the game state.
%% @param a PID - process id, and a state: Pid, _S = #state...
%% @returns ok.
send_game_state(Pid, _S = #state{
  game_state = GState,
  sess = Sess,
  board = Board,
  board_guesses = Guesses,
  round_guesses = Round
}) ->
  Msg = #{
    event => game_state_updated,
    game_state => GState,
    board => Board,
    board_guesses => Guesses,
    user_count => maps:size(Sess),
    guess_count => maps:size(Round)
    },
  grdl_sess_serv:send_message(Pid, Msg),
  ok.
