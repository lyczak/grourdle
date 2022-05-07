-module(grourdle_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


%% @doc Starts the Grourdle Application by initiating the server handling session for the Grourdle App.
%% @param _Type, tpye of session to be initiated.
%% @param _Args, arguments for the Application to start correctly.
%% @returns grourdle_sup:start_linke()., after initiating the server session, the function ends by calling
%% the start link function in grourdle_sup.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, grourdle, "index.html"}},
			{"/assets/[...]", cowboy_static, {priv_dir, grourdle, "assets"}},
			{"/scripts/[...]", cowboy_static, {priv_dir, grourdle, "scripts"}},
			{"/ws", grdl_ws_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(grdl_http_listener,
		[{port, 1312}],
		#{env => #{dispatch => Dispatch}}
	),
	grourdle_sup:start_link().
%% @doc Quick Function to stop the current state of the Application client/server session.
%% @param _State, the state of the Application.
%% @returns ok., a terminating atom.
stop(_State) ->
	ok.
