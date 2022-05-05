-module(grdl_guess_arbiter).

-export([choose_guess/1]).

%minimum(X, Y, Z)  ->
%  ValueList = [X, Y, Z],
%  lists:min(ValueList).

%edit_distance(String1, String2, Len1, Len2) ->
%  if
%    Len1 == 0 ->
%      Len2;
%    Len2 == 0 ->
%      Len1;
%    true ->
%      C1 = lists:sublist(String1, Len1, Len1),
%      C2 = lists:sublist(String2, Len2, Len2),
%
%      if
%        C1 == C2 -> edit_distance(String1, String2, Len1 - 1, Len2 - 1);
%        true -> 1 + minimum(edit_distance(String1, String2, Len1, Len2 - 1), edit_distance(String1, String2, Len1 - 1, Len2), edit_distance(String1, String2, Len1 - 1, Len2 - 1))
%      end
%  end.


edit_distance(First,Second) ->
  compare(First,Second).

compare(First,Second) ->
  if length(First) == 0 ->
    length(Second);
    length(Second) == 0 ->
      length(First);

    hd(First) == hd(Second) ->
      compare(tl(First),tl(Second));
    true -> 1 + lists:min([compare(First,tl(Second)),compare(tl(First),Second),compare(tl(First),tl(Second))])
  end.

list_to_variable([], OutList) -> OutList;
list_to_variable([E1 | EN], OutList) ->
  EN,
  Out = lists:append([E1 | EN], OutList),
  Out.

%pick_min_edit_distance_sum([], Index) -> Index;
%pick_min_edit_distance_sum([ED1 | EDN], Index)  ->
%  if
%    EDN < ED1 -> Index = Index+1;
%    true -> pick_min_edit_distance_sum(EDN, Index)
%  end.


count_edit_distances(G, []) ->
  G,
  0;
count_edit_distances(G, [Guess | NextGuess]) ->
  %%lists:append([edit_distance(G, Guess, length(G), length(Guess))], [count_edit_distances(G, NextGuess)]).
  edit_distance(G, Guess) + count_edit_distances(G, NextGuess).


build_tuple_val_list(List) when length(List) == 0 -> [];
build_tuple_val_list(List) ->
  [element(2, hd(List)) | build_tuple_val_list(tl(List))].

get_min_val(List) ->
  lists:min(build_tuple_val_list(List)).

make_min_tuple_list(List, MinVal) ->
  if length(List) =:= 0 -> [];
    element(2, hd(List)) == MinVal -> [hd(List)] ++ make_min_tuple_list(tl(List), MinVal);
    true -> make_min_tuple_list(tl(List), MinVal)
  end.

get_min_tuple(List) ->
  MinVal = get_min_val(List),
  TupleLst = make_min_tuple_list(List, MinVal),
  R0 = rand:uniform(length(TupleLst)),
  lists:nth(R0, TupleLst).

build_edit_distance_tuples([], OutList, FullList) ->
  FullList,
  OutList;
build_edit_distance_tuples([E1 | EN], OutList, FullList) ->
  Out = lists:append(OutList, [{E1, count_edit_distances(E1, FullList)}]),
  build_edit_distance_tuples(EN, Out, FullList).

choose_guess(Guesses = [First | _]) when is_binary(First) ->
  list_to_binary(
    choose_guess(
      lists:map(
        fun(G) -> binary_to_list(G) end,
        Guesses)));

%% @doc Pick/generate a Wordle guess from a list of several guesses.
choose_guess([Guess | NextGuess]) ->

  EList1 = [],
  EList = [],
  FullList = list_to_variable([Guess | NextGuess], EList1),
  RecursiveLst = build_edit_distance_tuples([Guess | NextGuess], EList, FullList),
  ChosenGuess = lists:nth(1, tuple_to_list(get_min_tuple(RecursiveLst))),
  ChosenGuess.

  %%MinTuple = get_min_tuple(RecursiveLst), %%uncomment these two lines to be able to check the minimum tuple that contains the chosen guess and its sum edit distances from the other guesses
  %%MinTuple.





