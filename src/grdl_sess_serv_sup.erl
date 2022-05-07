-module(grdl_sess_serv_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

%% @doc spawn and link to a new session supervisor.
start_link() ->
  io:format("sess_serv_sup start_link pid: ~p~n", [self()]),
  supervisor:start_link(?MODULE, []).

%% @doc initialize a session server supervisor with a session server child spec.
%% the session servers will be added dynamically by the pool server.
%% @param takes initialization values, if any.
%% @returns a tuple with supervisor information.
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
