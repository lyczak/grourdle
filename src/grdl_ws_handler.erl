-module(grdl_ws_handler).
-behaviour(cowboy_websocket).

-export([websocket_send/2]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {sess_pid, sess_ref}).

%%%===================================================================
%%% API
%%%===================================================================

websocket_send(WsPid, Msg) ->
  WsPid ! {send, Msg}.

%%%===================================================================
%%% cowboy_websocket
%%%===================================================================

init(Req, _State) ->
  io:format("handler init~n"),
  {cowboy_websocket, Req, #state{}, #{
    idle_timeout => 60000,
    max_frame_size => 8000000
  }}.


websocket_init(State) ->
  io:format("ws init! | pid: ~p | state ~p~n", [self(), State]),
  {ok, State}.


websocket_handle({text, Text}, State) ->
  io:format("ws> ~p~n", [Text]),
  Msg = jsx:decode(Text, [{labels, existing_atom}]),
  io:format("ws> ~p~n", [Msg]),
  {ok, NewState} = handle_message(Msg, State),
  {ok, NewState};
websocket_handle(_Frame, State) ->
  {ok, State}.

websocket_info({'DOWN', Ref, process, _Pid, _}, S = #state{sess_ref = SRef}) when Ref == SRef ->
  io:format("ws_handler my sess_serv went down! terminating ws connection~n"),
  {[
    {text, jsx:encode({error, "session server down"})},
    {close}
  ], S};
websocket_info({log, Text}, State) ->
  io:format("ws< ~p~n", [Text]),
  {[{text, Text}], State};
websocket_info({send, Msg}, State) ->
  io:format("ws< ~p~n", [Msg]),
  Text = jsx:encode(Msg),
  {reply, {text, Text}, State};
websocket_info(_Info, State) ->
  {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

handle_message(#{start_session := true}, S = #state{sess_pid = undefined}) ->
  {ok, Pid, SessId} = grdl_sess_pool_serv:start_session(),
  io:format("ws_handler start new session with SessId: ~p~n", [SessId]),
  Ref = monitor(process, Pid),
  grdl_sess_serv:bind_to_ws(Pid, self()),
  Msg = #{event => session_bound, sess_id => SessId},
  websocket_send(self(), Msg),
  {ok, S#state{sess_pid = Pid, sess_ref = Ref}}; % todo: maybe reduce code duplication here
handle_message(#{bind_session := SessId}, S = #state{sess_pid = undefined}) ->
  io:format("ws_handler bind to existing session:~n"),
  {ok, Pid} = grdl_sess_pool_serv:get_session(SessId), % todo: handle invalid SessId
  Ref = monitor(process, Pid),
  grdl_sess_serv:bind_to_ws(Pid, self()),
  Msg = #{event => session_bound, sess_id => SessId},
  websocket_send(self(), Msg),
  {ok, S#state{sess_pid = Pid, sess_ref = Ref}};
handle_message(Msg, S = #state{sess_pid = SPid}) when is_pid(SPid) ->
  io:format("ws_handler passing to session ~p~n", [Msg]),
  grdl_sess_serv:handle_message(SPid, Msg),
  {ok, S};
handle_message(Msg, State) ->
  io:format("ws_handler unknown message ~p~n", [Msg]),
  {ok, State}.

%%ws_send(Pid, SInterval) ->
%%  erlang:start_timer(SInterval, Pid, []);
