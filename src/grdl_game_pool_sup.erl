-module(grdl_game_pool_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

%% @doc spawn and link to a new game pool supervisor
start_link() ->
  io:format("game_pool_sup start_link~n"),
  supervisor:start_link(?MODULE, []).

%% @doc initialize a game pool supervisor with a game pool server
%% the game server supervisor will be added dynamically by the game pool server
init([]) ->
  MaxR = 1,
  MaxT = 3000,
  {ok, {{one_for_all, MaxR, MaxT}, [
    {
      game_pool_serv,
      {grdl_game_pool_serv, start_link, [self()]},
      permanent,
      5000, % shutdown time
      worker,
      [grdl_game_pool_serv]
    }
  ]}}.
