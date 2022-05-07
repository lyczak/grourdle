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

%% @doc spawn and link to a new session server with a given session id
%% @param SessId the session id uniquely identifying this new session
start_link(SessId) ->
  gen_server:start_link(?MODULE, [SessId], []).

%% @doc bind a session server to a given websocket handler
%% Binding should occur directly after the session server is initialized. This will
%% establish a line of communication whereby the the session can pass along messages
%% to the websocket handler to be sent to a web-browser client.
%% @param SsPid the pid of the session server to be bound
%% @param WsPid the pid of the websocket handler to bind to
%% @see grdl_ws_handler
bind_to_ws(SsPid, WsPid) ->
  gen_server:call(SsPid, {bind_to_socket, WsPid}).

%% @doc asynchronously handle a new message from a websocket handler
%% Messages are expected to be formatted according to the map syntax of the JSX library.
%% Messages correspond to events initiated by the web-browser client. These events can be
%% processed asynchronously and dont necessitate an immediate response or order-guarante.
%% @param SsPid the pid of the session server that should handle this message
%% @param Msg the JSX map-format message coming from the websocket handler
%% @see grdl_ws_handler
handle_message(SsPid, Msg) ->
  gen_server:cast(SsPid, {ws_message, Msg}).

%% @doc pass a websocket message along to the given websocket handler
%% Because other processes do not have direct access to websocket
%% handlers, it may be necessary for us to proxy websocket messages through a session server
%% which internally keeps track of the ws_handler pid in its state. This function forwards a
%% message to be formatted by JSX and passed out the corresponding websocket.
%% @param SsPid the pid of the session server forward through
%% @param Msg the JSX map-format message to be forwarded to the ws handler
%% @see grdl_ws_handler
%% @see grdl_game_serv
send_message(SsPid, Msg) ->
  gen_server:cast(SsPid, {send_message, Msg}).

%%%===================================================================
%%% gen_server
%%%===================================================================

%% @doc initialize a new sess_serv (via gen_server callback) with a given session id
%% Initialize the state with said session id.
init([SessId]) ->
  {ok, #state{sess_id = SessId}}.

% calls

%% @doc bind to a websocket
%% Create a new monitor to the websocket and update the state
%% with that monitor and the ws_handler's Pid
%% @see bind_to_ws/2
handle_call({bind_to_socket, WsPid}, _From, S = #state{}) ->
  Ref = erlang:monitor(process, WsPid),
  {reply, ok, S#state{ws_pid = WsPid, ws_ref = Ref}};

%% catchall reply ok to other messages
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

% casts

%% @doc respond to a websocket sess_echo message
%% Takes message with sess_echo atom mapped to a string, logs to console,
%% replies with sess_echo event message to the ws_handler. State is unchanged
%% @see handle_message/2
handle_cast({ws_message, _ = #{sess_echo := Echo}}, State = #state{ ws_pid = WsPid }) ->
  io:format("session echoing! ~p -> ~p~n", [self(), WsPid]),
  grdl_ws_handler:websocket_send(WsPid, #{event => sess_echo, sess_echo => Echo}),
  {noreply, State};

%% @doc handle a websocket set_username message
%% @see handle_message/2
%% Takes message with set_username atom mapped to a string. Updates state with new username.
handle_cast({ws_message, _ = #{set_username := Username}}, S = #state{}) ->
  {noreply, S#state{username = Username}};

%% @doc handle a websocket message to create a new game
%% @see handle_message/2
%% @see grdl_game_pool_serv
%% Takes message with join_game atom mapped to binary <<"new">> along
%% with undefined g_pid in state. Spawns a game,
%% binds to it as the owner, creates a monitor to the game, replies with
%% ws message indicating game is bound, updates state with game pid and monitor.
handle_cast({ws_message, _ = #{join_game := <<"new">>}}, S = #state{g_pid = undefined}) ->
  {ok, Pid, GameId} = grdl_game_pool_serv:start_game(),
  grdl_game_serv:bind_to_owner(Pid),
  Ref = erlang:monitor(process, Pid),
  send_message(self(), #{event => game_bound, game_id => GameId}),
  {noreply, S#state{g_pid = Pid, g_ref = Ref}};

%% @doc handle a websocket message to join an existing game
%% @see handle_message/2
%% @see grdl_game_pool_serv
%% Takes message with join_game atom mapped to binary along
%% with undefined g_pid in state. Searches for a game,
%% joins it, creates a monitor to the game, replies with
%% ws message indicating game is bound, updates state with game pid and monitor.
%% If game is not found, replies with game_unbound ws event message stating not found.
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

%% @doc handle a websocket message to start the bound game
%% @see handle_message/2
%% @see grdl_game_serv:start_game/1
%% Takes message with start_game atom mapped to anything, along with defined g_pid
%% in state. Starts the game. State unchanged.
handle_cast({ws_message, _ = #{start_game := _}}, S = #state{g_pid = Pid}) ->
  grdl_game_serv:start_game(Pid),
  {noreply, S};

%% @doc handle a websocket message to submit a guess to the bound game
%% @see handle_message/2
%% @see grdl_game_serv:submit_guess/2
%% Takes message with guess atom mapped to anything, along with defined g_pid
%% in state. Submits the guess. State unchanged.
handle_cast({ws_message, _ = #{guess := Guess}}, S = #state{g_pid = Pid}) ->
  grdl_game_serv:submit_guess(Pid, Guess),
  {noreply, S};

%% @doc forward a websocket message to the bound websocket handler
%% @see send_message/2
%% @see grdl_ws_handler:websocket_send/2
%% Takes message, along with defined ws_pid in state. Sends the message through the websocket.
%% State unchanged.
handle_cast({send_message, Msg}, S = #state{ ws_pid = Pid }) ->
  grdl_ws_handler:websocket_send(Pid, Msg),
  {noreply, S};

%% catchall ignore unrecognized casts
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

% etc

%% @doc handle down message (presumably from one of the monitors we created)
%% Takes a 'DOWN' message, along with g_pid and ws_pid in state.
%% Matches on the Pid of the downed process. Sends game_unbound ws message if
%% the game has gone down and kills self normally if the ws handler has gone down.
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

%% catchall ignore unrecognized info messages
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @doc cleanly terminate this session server
terminate(_Reason, _State = #state{}) ->
  ok.

%% @doc handle new code version loaded at runtime
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
