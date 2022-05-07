-module(grourdle_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc spawn and link to new grourdle_sup
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc initialize children templates in a one-for-one scheme, max one restart per 3 sec
%% Child specs include one @see grdl_sess_pool_sup and one @see grdl_game_pool_sup
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
