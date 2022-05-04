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

% spawn and link to a new game pool server with a given pool supervisor pid
start_link(PoolSup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {PoolSup}, []).

% start a new game server and get {ok, GameServerPid, GameServerIdCode}
start_game() ->
  gen_server:call(?SERVER, {start_game}).

% get a game server given it's Id Code and return {ok, GameServerPid}
% if not found return {error, game_not_found, GameId}
get_game(GameId) ->
  gen_server:call(?SERVER, {get_game, GameId}).

%%%===================================================================
%%% gen_server
%%%===================================================================

% initialize a game pool server with a given supervisor pid
% set initial state and then handle message to set supervisor (who many not have started yet)
init({PoolSup}) ->
  self() ! {start_server_supervisor, PoolSup},
  {ok, #state{refs = gb_sets:empty()}}.

% == call ==

% see start_game/0
% start a new game server, generate a new id code, update the state and return
handle_call({start_game}, _From, S = #state{sup = Sup, gmap = GMap, refs = Refs}) ->
  GameId = list_to_binary(grdl_utils:game_id(GMap)),
  io:format("game_pool_serv spawning game_serv~n"),
  {ok, Pid} = supervisor:start_child(Sup, []), % [] is where we'd add sess_serv args
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid, GameId},
    S#state{gmap = GMap#{GameId => Pid}, refs = gb_sets:add(Ref,Refs)}
  };

% see get_game/1
% get a game from the map of games and return it or return error if not found
handle_call({get_game, GameId}, _From, S = #state{gmap = GMap}) ->
  io:format("game_pool_serv handle_call get_game~n"),
  case maps:find(GameId, GMap) of
    {ok, Pid} -> {reply, {ok, Pid}, S};
    error ->
      {reply, {error, game_not_found, GameId}, S}
  end;

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%  == cast ==

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% == info ==

% handle process monitor down signal
% this means one of the game servers went down
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

% start the server supervisor after us and our supervisor have initialized
% server supervisor should be a child of our parent pool supervisor
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

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

% etc
terminate(_Reason, _State = #state{}) ->
  ok.


code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

