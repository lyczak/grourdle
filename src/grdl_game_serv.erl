-module(grdl_game_serv).
-behaviour(gen_server).

-export([start_link/0, bind_to_owner/1, join_game/1, leave_game/2,
  start_game/1, submit_guess/2, end_round/1, end_game/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {
  owner, % owner's session pid for easy access
  sess, % #{Pid1 => Monitor1 ... PidN => MonitorN}
  word, % the game has a secret word, doesnt it?
  game_state, % waiting, active,
  board, % [{Guess1, [Color1, Color2 ... Color5]} ... {GuessN, [Color1, Color2 ... Color5]}]
  round_guesses % #{Pid1 => Guess1 ... PidN => GuessN}
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

end_game(GsPid) ->
  gen_server:call(GsPid, {end_game}). % todo: handle end game

%%%===================================================================
%%% gen_server
%%%===================================================================

% initialize the state of a new game server
init([]) ->
  {ok, #state{sess = gb_sets:empty(), game_state = waiting, board = [], round_guesses = #{}}}.

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
handle_call({join_game}, {Pid, _Tag}, S = #state{sess = Refs}) ->
  Ref = erlang:monitor(process, Pid),
  {reply, ok, S#state{
    sess = Refs#{Pid => Ref}
  }};

% see leave_game/1
% remove existing monitor and sess pid entry
handle_call({leave_game}, {Pid, _Tag}, S = #state{sess = Refs}) ->
  Ref = maps:get(Pid, Refs),
  erlang:demonitor(Ref),
  {reply, ok, S#state{
    sess = maps:remove(Ref, Refs)
  }};

% see start_game/1
% only run when called by owner and game state is waiting.
% set game_state to active, clear board and round guesses
handle_call({start_game}, {Pid, _Tag},
    S = #state{owner = Owner, game_state = waiting})
  when Pid == Owner orelse Pid == self() ->
  Word = "tests", % TODO: choose a word
  {reply, ok, S#state{
    game_state = active,
    word = Word,
    board = [],
    round_guesses = #{}
  }};

% see submit_guess/2
% if session's guess not exists, add it
handle_call({submit_guess, Guess}, {Pid, _Tag}, S = #state{round_guesses = Round}) ->
  case maps:find(Pid, Round) of
    {ok, _} -> {reply, ok, S};
    error -> {reply, ok, S#state{
      sess = S#{Pid => Guess}
    }}
  end;

% see end_round/1
% if no guesses end game, otherwise, pick a guess and update the board
% then prepare the state for the next round
handle_call({end_round}, {Pid, _Tag},
    S = #state{word = Word, board = Board, round_guesses = Round, owner = Owner})
  when Pid == Owner orelse Pid == self() ->
  Guesses = maps:values(Round),
  case Guesses of
    [] -> end_game(self());
    _ ->
      G = guess_arbiter:choose_guess(Guesses),
      R = grdl_wordle:check_guess(G, Word),
      {reply, ok, S#state{
        round_guesses = [Board | R]
      }}
  end;

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% casts
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% etc
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
