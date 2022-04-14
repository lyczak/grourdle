-module(grdl_sess_serv).
-behaviour(gen_server).

-export([start_link/0, bind_to_ws/2, handle_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {ws_pid, ws_ref, game, username}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

bind_to_ws(SsPid, WsPid) ->
  gen_server:call(SsPid, {bind_to_socket, WsPid}).

handle_message(SsPid, Msg) ->
  gen_server:cast(SsPid, {ws_message, Msg}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
  {ok, #state{}}.

% calls
handle_call({bind_to_socket, WsPid}, _From, S = #state{}) ->
  Ref = erlang:monitor(process, WsPid),
  {reply, ok, S#state{ws_pid = WsPid, ws_ref = Ref}};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% casts
handle_cast({ws_message, _ = #{sess_echo := Echo}}, State = #state{ ws_pid = WsPid }) ->
  io:format("session echoing! ~p -> ~p~n", [self(), WsPid]),
  grdl_ws_handler:websocket_send(WsPid, #{event => sess_echo, sess_echo => Echo}),
  {noreply, State};

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
