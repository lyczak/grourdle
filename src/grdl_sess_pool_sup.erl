-module(grdl_sess_pool_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  io:format("sess_pool_sup start_link~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  MaxR = 1,
  MaxT = 3000,
  {ok, {{one_for_all, MaxR, MaxT}, [
    {
      sess_pool_serv,
      {grdl_sess_pool_serv, start_link, [self()]},
      permanent,
      5000, % shutdown time
      worker,
      [grdl_sess_pool_serv]
    }
  ]}}.
