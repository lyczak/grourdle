-module(grdl_utils).

-export([base64_id/1, unique_base64_id/2, game_id/1]).

% get a cryptographically secure id in base64 that's this many bytes long
base64_id(Bytes) ->
  base64:encode(crypto:strong_rand_bytes(Bytes)).

% get a cryptographically secure id in base64 that's this many bytes long
% the id must be a unique key within the provided map
unique_base64_id(Bytes, Map) ->
  Id = base64_id(Bytes),
  case maps:find(Id, Map) of
    {ok, _} -> unique_base64_id(Bytes, Map);
    error -> Id
  end.

game_id(Map) ->
  Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  Id = lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(Chars)), Chars)] ++ Acc
              end, [], lists:seq(1, 4)),
  case maps:is_key(Id, Map) of
    true -> game_id(Map);
    false -> Id
  end.