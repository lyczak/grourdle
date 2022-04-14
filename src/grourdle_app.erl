-module(grourdle_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).



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

stop(_State) ->
	ok.
