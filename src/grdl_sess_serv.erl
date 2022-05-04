-module(grdl_sess_serv).
-behaviour(gen_server).

-export([start_link/1, bind_to_ws/2, handle_message/2, send_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {ws_pid, ws_ref, sess_id, g_pid, g_ref, username}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessId) ->
  gen_server:start_link(?MODULE, [SessId], []).

bind_to_ws(SsPid, WsPid) ->
  gen_server:call(SsPid, {bind_to_socket, WsPid}).

handle_message(SsPid, Msg) ->
  gen_server:cast(SsPid, {ws_message, Msg}).

send_message(SsPid, Msg) ->
  gen_server:cast(SsPid, {send_message, Msg}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([SessId]) ->
  {ok, #state{sess_id = SessId}}.

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

handle_cast({ws_message, _ = #{set_username := Username}}, S = #state{}) ->
  {noreply, S#state{username = Username}};

handle_cast({ws_message, _ = #{join_game := <<"new">>}}, S = #state{g_pid = undefined}) ->
  {ok, Pid, GameId} = grdl_game_pool_serv:start_game(),
  grdl_game_serv:bind_to_owner(Pid),
  Ref = erlang:monitor(process, Pid),
  send_message(self(), #{event => game_bound, game_id => GameId}),
  {noreply, S#state{g_pid = Pid, g_ref = Ref}};

handle_cast({ws_message, _ = #{join_game := GameId}}, S = #state{g_pid = undefined}) ->
  case grdl_game_pool_serv:get_game(GameId) of
    {error, game_not_found, _} ->
      send_message(self(), #{event => game_unbound, reason => game_not_found}),
      {noreply, S};
    {ok, Pid} ->
      grdl_game_serv:join_game(Pid),
      Ref = erlang:monitor(process, Pid),
      send_message(self(), #{event => game_bound, game_id => GameId}),
      {noreply, S#state{g_pid = Pid, g_ref = Ref}}
  end;

handle_cast({ws_message, _ = #{start_game := _}}, S = #state{g_pid = Pid}) ->
  grdl_game_serv:start_game(Pid),
  {noreply, S};

handle_cast({ws_message, _ = #{guess := Guess}}, S = #state{g_pid = Pid}) ->
  grdl_game_serv:submit_guess(Pid, Guess),
  {noreply, S};

handle_cast({send_message, Msg}, S = #state{ ws_pid = Pid }) ->
  grdl_ws_handler:websocket_send(Pid, Msg),
  {noreply, S};

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% etc
handle_info({'DOWN', _Ref, process, Pid, Reason}, S = #state{g_pid = Game, ws_pid = WS}) ->
  case Pid of
    Game ->
      io:format("i'm a sess_serv and my game_serv just went down~n"),
      grdl_ws_handler:websocket_send(WS, #{event => game_unbound, reason => Reason}),
      {noreply, S};
    WS ->
      io:format("i'm a sess_serv and my ws_handler just went down~n"),
      exit(normal),
      {noreply, S};
    _ -> {noreply, S}
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
