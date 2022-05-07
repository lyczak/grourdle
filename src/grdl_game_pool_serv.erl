-module(grdl_game_pool_serv).
-behaviour(gen_server).

-export([start_link/1, start_game/0, get_game/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(IDLENGTH, 12). % bytes
-record(state, {sup, refs, gmap = #{}}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc spawn and link to a new game pool server with a given pool supervisor pid.
%% @param Pool Supervisor pid
start_link(PoolSup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {PoolSup}, []).

%% @doc start a new game server and get {ok, GameServerPid, GameServerIdCode}
start_game() ->
  gen_server:call(?SERVER, {start_game}).

%% @doc get a game server given it's Id Code and return {ok, GameServerPid}
%% if not found return {error, game_not_found, GameId}.
%% @param Game server pid
%% @returns returns {ok, GameServerPid} or if not found returns {error, game_not_found, GameId}
get_game(GameId) ->
  gen_server:call(?SERVER, {get_game, GameId}).

%%%===================================================================
%%% gen_server
%%%===================================================================

%% @doc initialize a game pool server with a given supervisor pid
%% set initial state and then handle message to set supervisor (who many not have started yet)
%% @param PoolSup Pool Supervisor Pid
%% @returns {ok, #state{refs = gb_sets:empty()}}
init({PoolSup}) ->
  self() ! {start_server_supervisor, PoolSup},
  {ok, #state{refs = gb_sets:empty()}}.

% == call ==

%% @doc @see start_game/0
%% start a new game server, generate a new id code, update the state and return
%% @param message = {start_game}, state
%% @returns reply to caller with {ok, Pid, gameId}, update state, add a monitor
handle_call({start_game}, _From, S = #state{sup = Sup, gmap = GMap, refs = Refs}) ->
  GameId = list_to_binary(grdl_utils:game_id(GMap)),
  io:format("game_pool_serv spawning game_serv~n"),
  {ok, Pid} = supervisor:start_child(Sup, []), % [] is where we'd add sess_serv args
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid, GameId},
    S#state{gmap = GMap#{GameId => Pid}, refs = gb_sets:add(Ref,Refs)}
  };

%% @doc @see get_game/1
%% get a game from the map of games and return it or return error if not found
%% @param message {get_game, GameID}, State
%% @returns reply {reply, {ok, Pid}, S} if game found or {reply, {error, game_not_found, GameId}, S} if game not found
handle_call({get_game, GameId}, _From, S = #state{gmap = GMap}) ->
  io:format("game_pool_serv handle_call get_game~n"),
  case maps:find(GameId, GMap) of
    {ok, Pid} -> {reply, {ok, Pid}, S};
    error ->
      {reply, {error, game_not_found, GameId}, S}
  end;

%% @doc reply ok to call requests that have not been overriden
%% @param State
%% @returns {reply, ok, State}
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%  == cast ==
%% @doc noreply to cast requests that have not been overriden
%% @param State
%% @returns {noreply, State}
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% == info ==

%% @doc handle process monitor down signal
%% this means one of the game servers went down
%% @param {'DOWN', Ref, process, Pid, _} , State
%% @returns noreply, updated state with game server removed
handle_info({'DOWN', Ref, process, Pid, _}, S = #state{refs = Refs, gmap = GMap}) ->
  case gb_sets:is_element(Ref, Refs) of
    true ->
      io:format("i'm a pool_serv and my game_serv just went down~n"),
      erlang:demonitor(Ref),
      {noreply, S#state{
        refs = gb_sets:delete(Ref, Refs),
        gmap = maps:filter(fun(_, P) -> P == Pid end, GMap)
      }};
    false ->
      {noreply, S}
  end;

%% @doc start the server supervisor after us and our supervisor have initialized
%% server supervisor should be a child of our parent pool supervisor
%% @param message {start_server_supervisor, PoolSup}, State
%% @returns updated state
handle_info({start_server_supervisor, PoolSup}, S = #state{}) ->
  io:format("game_pool_serv starting game_serv_sup state~n"),
  {ok, Pid} = supervisor:start_child(PoolSup, {
    game_serv_sup,
    {grdl_game_serv_sup, start_link, []},
    temporary,
    10000, % shutdown timeout
    supervisor,
    [grdl_game_serv_sup]
  }),
  link(Pid),
  {noreply, S#state{sup=Pid}};

%% @doc handle generic info call
%% @param State
%% @returns {noreply, State}
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

% etc
%% @doc boilerplate terminate
%% @param State
%% @returns ok
terminate(_Reason, _State = #state{}) ->
  ok.

%% @doc boilerplate code change
%% @param State
%% @returns {ok, State}
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

