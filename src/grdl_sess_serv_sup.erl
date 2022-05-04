-module(grdl_sess_serv_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  io:format("sess_serv_sup start_link pid: ~p~n", [self()]),
  supervisor:start_link(?MODULE, []).

init([]) ->
  MaxR = 1,
  MaxT = 3000,
  {ok, {{simple_one_for_one, MaxR, MaxT}, [
    {
      sess_serv,
      {grdl_sess_serv, start_link, []},
      temporary,
      5000,
      worker,
      [grdl_sess_serv]
    }
  ]}}.
