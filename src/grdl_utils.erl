-module(grdl_utils).

-export([base64_id/1, unique_base64_id/2, test/0]).

base64_id(Bytes) ->
  base64:encode(crypto:strong_rand_bytes(Bytes)).

unique_base64_id(Bytes, Map) ->
  Id = base64_id(Bytes),
  case maps:find(Id, Map) of
    {ok, _} -> unique_base64_id(Bytes, Map);
    error -> Id
  end.

test() ->
  receive
    continue -> test();
    terminate -> ok
  end.