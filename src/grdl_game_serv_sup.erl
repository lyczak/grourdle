-module(grdl_game_serv_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

% spawn and link to a new game server supervisor
start_link() ->
  io:format("game_serv_sup start_link pid: ~p~n", [self()]),
  supervisor:start_link(?MODULE, []).

% initialize a game server supervisor with a game server child spec
% the game servers will be added dynamically by the pool server
init([]) ->
  MaxR = 1,
  MaxT = 3000,
  {ok, {{simple_one_for_one, MaxR, MaxT}, [
    {
      game_serv,
      {grdl_game_serv, start_link, []},
      temporary,
      5000,
      worker,
      [grdl_game_serv]
    }
  ]}}.
