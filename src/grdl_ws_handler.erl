-module(grdl_ws_handler).
-behaviour(cowboy_websocket).

-export([websocket_send/2]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {sess_pid, sess_ref}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc send a JSX map-format websocket message through a ws handler
%% @param WsPid the Pid of the websocket handler who should send the message
%% @param Msg the JSX map-format message the websocket should send.
websocket_send(WsPid, Msg) ->
  WsPid ! {send, Msg}.

%%%===================================================================
%%% cowboy_websocket
%%%===================================================================

%% @doc callback to initialize a new grdl_ws_handler as a cowboy_websocket handler
%% state is initialized to an empty state record and connection params are set.
init(Req, _State) ->
  io:format("handler init~n"),
  {cowboy_websocket, Req, #state{}, #{
    idle_timeout => 60000,
    max_frame_size => 8000000
  }}.


%% @doc callback to initialize the ws handler after @see init/2
websocket_init(State) ->
  io:format("ws init! | pid: ~p | state ~p~n", [self(), State]),
  {ok, State}.

%% @doc callback to handle incoming text-based websocket message.
%% Message is logged and parsed from JSON using JSX (looking for existing atoms).
%% The message is handled via a helper who returns the new state which we return.
websocket_handle({text, Text}, State) ->
  io:format("ws> ~p~n", [Text]),
  Msg = jsx:decode(Text, [{labels, existing_atom}]),
  io:format("ws> ~p~n", [Msg]),
  {ok, NewState} = handle_message(Msg, State),
  {ok, NewState};

%% handle (ignore) all other messages
websocket_handle(_Frame, State) ->
  {ok, State}.

%% @doc handle monitor 'DOWN' message (presumably from a downed session server)
%% Respond with a JSX-encoded error message and close the connection.
websocket_info({'DOWN', Ref, process, _Pid, _}, S = #state{sess_ref = SRef}) when Ref == SRef ->
  io:format("ws_handler my sess_serv went down! terminating ws connection~n"),
  {[
    {text, jsx:encode({error, "session server down"})},
    {close}
  ], S};

%% @doc handle info log message
%% Send raw outgoing text through websocket without JSX preprocessing.
websocket_info({log, Text}, State) ->
  io:format("ws< ~p~n", [Text]),
  {[{text, Text}], State};

%% @doc handle info send message
%% Encode message as JSON via JSX and send out the websocket.
websocket_info({send, Msg}, State) ->
  io:format("ws< ~p~n", [Msg]),
  Text = jsx:encode(Msg),
  {reply, {text, Text}, State};

%% catchall handle (ignore) any other info message
websocket_info(_Info, State) ->
  {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

%% @doc handle a ws message to start a new session
%% Takes map mapping start_session atom to true along with undefined session server pid.
%% Starts a new session via grdl_sess_pool_serv, creates a new
%% monitor, replies with ws message session_bound and the session id. Updates the state
%% with the new session server pid and monitor reference.
%% @see grdl_sess_pool_serv:start_session/0
handle_message(#{start_session := true}, S = #state{sess_pid = undefined}) ->
  {ok, Pid, SessId} = grdl_sess_pool_serv:start_session(),
  io:format("ws_handler start new session with SessId: ~p~n", [SessId]),
  Ref = monitor(process, Pid),
  grdl_sess_serv:bind_to_ws(Pid, self()),
  Msg = #{event => session_bound, sess_id => SessId},
  websocket_send(self(), Msg),
  {ok, S#state{sess_pid = Pid, sess_ref = Ref}};

%% @doc handle a ws message to bind to an existing session
%% Takes map mapping bind_session atom to a session id along with undefined
%% session server pid in the state. Tries to bind to the session via
%% @see grdl_sess_pool_serv:get_session/1
%% Creates a new monitor, replies with ws message session_bound and the session id.
%% Updates the state with the new session server pid and monitor reference.
%% If the session id is invalid, this process will crash and the socket will close.
handle_message(#{bind_session := SessId}, S = #state{sess_pid = undefined}) ->
  io:format("ws_handler bind to existing session:~n"),
  {ok, Pid} = grdl_sess_pool_serv:get_session(SessId),
  Ref = monitor(process, Pid),
  grdl_sess_serv:bind_to_ws(Pid, self()),
  Msg = #{event => session_bound, sess_id => SessId},
  websocket_send(self(), Msg),
  {ok, S#state{sess_pid = Pid, sess_ref = Ref}};

%% @doc handle a ws keepalive message (take no action)
handle_message(#{keepalive := _}, S = #state{}) ->
  {ok, S};

%% @doc pass along unhandled ws messages to a session server if one is bound
%% @see grdl_sess_serv:handle_message/2
handle_message(Msg, S = #state{sess_pid = SPid}) when is_pid(SPid) ->
  io:format("ws_handler passing to session ~p~n", [Msg]),
  grdl_sess_serv:handle_message(SPid, Msg),
  {ok, S};

%% catchall handle unknown message with sess_pid undefined
handle_message(Msg, State) ->
  io:format("ws_handler unknown message ~p~n", [Msg]),
  {ok, State}.

%%ws_send(Pid, SInterval) ->
%%  erlang:start_timer(SInterval, Pid, []);
