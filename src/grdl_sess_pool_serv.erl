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

start_link(PoolSup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {PoolSup}, []).

% to be called from websocket process
start_session() ->
  gen_server:call(?SERVER, {start_session}).

get_session(SessId) ->
  gen_server:call(?SERVER, {get_session, SessId}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init({PoolSup}) ->
  self() ! {start_server_supervisor, PoolSup},
  {ok, #state{refs = gb_sets:empty()}}.

% call
handle_call({start_session}, _From, S = #state{sup = Sup, smap = SMap, refs = Refs}) ->
  SessId = grdl_utils:unique_base64_id(?IDLENGTH, SMap),
  io:format("sess_pool_serv spawning sess_serv~n"),
  {ok, Pid} = supervisor:start_child(Sup, [SessId]),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid, SessId},
    S#state{smap = SMap#{SessId => Pid}, refs = gb_sets:add(Ref,Refs)}
  };

handle_call({get_session, SessId}, _From, S = #state{smap = SMap}) ->
  io:format("sess_pool_serv handle_call get_session~n"),
  case maps:find(SessId, SMap) of
    {ok, Pid} -> {reply, {ok, Pid}, S};
    error ->
      {reply, {error, session_not_found, SessId}, S}
  end;

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% cast
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% info
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

