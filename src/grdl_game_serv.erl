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

% start and link to a new game server
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% make the calling session process the owner of this game server
% there are certain actions that only the owner can perform
bind_to_owner(GsPid) ->
  gen_server:call(GsPid, {bind_to_session}).

% join the calling session process into this game server
% do not call this from the owner session, instead use bind_to_owner
join_game(GsPid) ->
  gen_server:call(GsPid, {join_game}).

% remove the calling session process from this game server.
% any session can call this but if the owner leaves, it will terminate the game server
leave_game(GsPid, Reason) ->
  gen_server:call(GsPid, {leave_game, Reason}).

% transition from the waiting state to start a new game.
% only the owner session can call this.
start_game(GsPid) ->
  gen_server:call(GsPid, {start_game}).

% submit a guess on behalf of the calling session process.
% existing guesses will not be changed.
% this should only be called during an active round.
submit_guess(GsPid, Guess) ->
  gen_server:call(GsPid, {submit_guess, Guess}).

% end the current round by choosing a guess and evaluating it
% against the secret word. this should only be called during an active round.
end_round(GsPid) ->
  gen_server:call(GsPid, {end_round}).

%%%===================================================================
%%% gen_server
%%%===================================================================

% initialize the state of a new game server
init([]) ->
  {ok, #state{
    sess = #{},
    game_state = waiting,
    board = [],
    board_guesses = [],
    round_guesses = #{}}}.

% == calls ==

% see bind_to_owner/1
% create a new monitor and add it to the sess refs. add owner pid to state
handle_call({bind_to_session}, {SsPid, _Tag}, S = #state{sess = Refs}) ->
  Ref = erlang:monitor(process, SsPid),
  {reply, ok, S#state{
    owner = SsPid,
    sess = Refs#{SsPid => Ref}
  }};

% see join_game/1
% create a new monitor and add it to the sess refs.
% tells all users a user has joined
handle_call({join_game}, {Pid, _Tag}, S1 = #state{sess = Refs}) ->
  Ref = erlang:monitor(process, Pid),
  UserCount = maps:size(Refs) + 1,
  S2 = S1#state{
    sess = Refs#{Pid => Ref}
  },
  broadcast(#{event => game_joined, user_count => UserCount}, S2),
  send_game_state(Pid, S2),
  {reply, ok, S2};

% see leave_game/1
% remove existing monitor and sess pid entry
% tells all users a user has left
handle_call({leave_game}, {Pid, _Tag}, S = #state{sess = Refs}) ->
  Ref = maps:get(Pid, Refs),
  erlang:demonitor(Ref),
  UserCount = maps:size(Refs) - 1,
  broadcast(#{event => game_left, user_count => UserCount}, S),
  {reply, ok, S#state{
    sess = maps:remove(Ref, Refs)
  }};

% see start_game/1
% only run when called by owner and game state is waiting.
% set game_state to active, clear board and round guesses
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

% see submit_guess/2
% if session's guess not exists, add it
% tells all users a guess has been submitted
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

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% casts
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% etc

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

%ends the game by setting game_state to waiting, informs all users
handle_info({end_game, Reason},
    S = #state{board = Board, game_state = active}) ->
  io:format("game_serv ending game for reason ~p~n", [Reason]),
  broadcast(#{event => game_ended, reason => Reason, board => Board}, S),
  {noreply, S#state{
    game_state = waiting
  }};

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

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%helper func that given state and message, messages all session servers
broadcast(Msg, _S = #state{sess = Sess}) ->
  maps:foreach(fun(Pid,_) -> grdl_sess_serv:send_message(Pid, Msg) end, Sess),
  ok.

%% sends some information about the current game state to a given session
%% used when the user's session is presumed to be out-of-sync with the game state
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
