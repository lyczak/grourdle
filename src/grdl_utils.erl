-module(grdl_utils).

-export([base64_id/1, unique_base64_id/2]).

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
