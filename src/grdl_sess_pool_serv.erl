-module(grdl_sess_pool_serv).
-behaviour(gen_server).

-export([start_link/1, start_session/0, get_session/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(IDLENGTH, 12). % bytes
-record(state, {sup, refs, smap = #{}}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc spawn and link to a new session pool server with a given pool supervisor pid.
%% @param PoolSup Pid
start_link(PoolSup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {PoolSup}, []).

% to be called from websocket process
%% @doc start a new game server and get {ok, SessionServerPid, SessionServerIdCode}
start_session() ->
  gen_server:call(?SERVER, {start_session}).

%% @doc retrieve a session server with session server ID
%% @param SessId session id
%% @returns {ok, SessionServerPid} or if not found returns {error, session_not_found, SessId}
get_session(SessId) ->
  gen_server:call(?SERVER, {get_session, SessId}).

%%%===================================================================
%%% gen_server
%%%===================================================================

%% @doc initiate a pool supervisor
%% @param {PoolSup} Pid
%% @returns {ok, #state{refs = gb_sets:empty()}}
init({PoolSup}) ->
  self() ! {start_server_supervisor, PoolSup},
  {ok, #state{refs = gb_sets:empty()}}.

% call
%% @doc @see start_session/0
%% start a new session server, generate a new id code, update the state and return
%% @param message = {start_session}, state
%% @returns reply to caller with {ok, Pid, gameId}, update state, add a monitor
handle_call({start_session}, _From, S = #state{sup = Sup, smap = SMap, refs = Refs}) ->
  SessId = grdl_utils:unique_base64_id(?IDLENGTH, SMap),
  io:format("sess_pool_serv spawning sess_serv~n"),
  {ok, Pid} = supervisor:start_child(Sup, [SessId]),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid, SessId},
    S#state{smap = SMap#{SessId => Pid}, refs = gb_sets:add(Ref,Refs)}
  };

%% @doc @see get_session/1
%% get a session from the map of games and return it or return error if not found
%% @param message {get_session, SessId}, State
%% @returns reply {reply, {ok, Pid}, S} if sess found or {reply, {error, game_not_found, SessId}, S} if sess not found
handle_call({get_session, SessId}, _From, S = #state{smap = SMap}) ->
  io:format("sess_pool_serv handle_call get_session~n"),
  case maps:find(SessId, SMap) of
    {ok, Pid} -> {reply, {ok, Pid}, S};
    error ->
      {reply, {error, session_not_found, SessId}, S}
  end;

%% @doc reply ok to call requests that have not been overriden
%% @param State
%% @returns {reply, ok, State}
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% cast
%% @doc noreply to cast requests that have not been overriden
%% @param State
%% @returns {noreply, State}
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% info
%% @doc handle process monitor down signal
%% this means one of the game servers went down
%% @param {'DOWN', Ref, process, Pid, _} , State
%% @returns noreply, updated state with game server removed
handle_info({'DOWN', Ref, process, Pid, _}, S = #state{refs = Refs, smap = SMap}) ->
  case gb_sets:is_element(Ref, Refs) of
    true ->
      io:format("i'm a pool_serv and my sess_serv just went down~n"),
      erlang:demonitor(Ref),
      {noreply, S#state{
        refs = gb_sets:delete(Ref, Refs),
        smap = maps:filter(fun(_, P) -> P == Pid end, SMap)
      }};
    false ->
      {noreply, S}
  end;

%% @doc start the server supervisor after us and our supervisor have initialized
%% server supervisor should be a child of our parent pool supervisor
%% @param message {start_server_supervisor, PoolSup}, State
%% @returns updated state
handle_info({start_server_supervisor, PoolSup}, S = #state{}) ->
  io:format("sess_pool_serv starting sess_serv_sup state: ~p~n", [S]),
  {ok, Pid} = supervisor:start_child(PoolSup, {
    sess_serv_sup,
    {grdl_sess_serv_sup, start_link, []},
    temporary,
    10000, % shutdown timeout
    supervisor,
    [grdl_sess_serv_sup]
  }),
  link(Pid),
  io:format("sess_pool_serv started sess_serv_sup! state: ~p~n", [S]), % todo: remove debug prints
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

