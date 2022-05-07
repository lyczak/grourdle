-module(grdl_utils).

-export([base64_id/1, unique_base64_id/2, game_id/1]).

%% @doc Get a cryptographically secure id in base64 that's this many bytes long.
%% @param Bytes, the length of the secure id.
%% @return encode(crypto:strong_rand_bytes(Bytes))., a secure id that is encrypted in base64.
base64_id(Bytes) ->
  base64:encode(crypto:strong_rand_bytes(Bytes)).

%% @doc Get a cryptographically secure id in base64 that's this many bytes long.
%% In addition, the id must be a unique key within the provided map.
%% @param Bytes, length of id.
%% @param Map, the Map that the given key is within - also unique within this Map.
%% @returns either the new Id from base65_id(Bytes) or the unique Id found in the Map parameter.
unique_base64_id(Bytes, Map) ->
  Id = base64_id(Bytes),
  case maps:find(Id, Map) of
    {ok, _} -> unique_base64_id(Bytes, Map);
    error -> Id
  end.

%% @doc Function that returns the Id of the current game or a new game Id if the game is not within the specified Map.
%% @param Map, the specified Map of games mapped to unique Ids.
%% @returns Either the Id of a new Game, or the uique Id of the Game within the Map parameter.
game_id(Map) ->
  Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  Id = lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(Chars)), Chars)] ++ Acc
              end, [], lists:seq(1, 4)),
  case maps:is_key(Id, Map) of
    true -> game_id(Map);
    false -> Id
  end.