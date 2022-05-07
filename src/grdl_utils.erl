-module(grdl_utils).

-export([base64_id/1, unique_base64_id/2, game_id/1]).

%% @doc Get a cryptographically secure id in base64 that's this many bytes long.
%% @param Bytes, the length of the secure id.
%% @returns a base64 encoded id
base64_id(Bytes) ->
  base64:encode(crypto:strong_rand_bytes(Bytes)).

%% @doc Get a cryptographically secure id in base64 that's this many bytes long.
%% In addition, the id must be a unique key within the provided map.
%% @param Bytes, length of id.
%% @param Map, the Map that the given key is within - also unique within this Map.
%% @returns the unique base64 encoded id
unique_base64_id(Bytes, Map) ->
  Id = base64_id(Bytes),
  case maps:find(Id, Map) of
    {ok, _} -> unique_base64_id(Bytes, Map);
    error -> Id
  end.

%% @doc generates a new four-character game id unique within the given map
%% @param Map, the specified Map whose keys are game ids
%% @returns a new unique four character game id
game_id(Map) ->
  Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  Id = lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(Chars)), Chars)] ++ Acc
              end, [], lists:seq(1, 4)),
  case maps:is_key(Id, Map) of
    true -> game_id(Map);
    false -> Id
  end.