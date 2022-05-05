-module(grourdle_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc spawn and link to <?>
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc intilize children templates in a one-for-one scheme, make restarts 1 per 3 seconds.
init([]) ->
	MaxR = 1,
	MaxT = 3000,
	{ok, {{one_for_one, MaxR, MaxT}, [
		{
			sess_pool,
			{grdl_sess_pool_sup, start_link, []},
			permanent,
			5000, % shutdown time
			worker,
			[grdl_sess_pool_sup]
		},
		{
			game_pool,
			{grdl_game_pool_sup, start_link, []},
			permanent,
			5000, % shutdown time
			worker,
			[grdl_game_pool_sup]
		}
	]}}.
