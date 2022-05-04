-module(guess_arbiter).

-import(lists,[min/1]).

-export([minimum/3, list_to_variable/2, pick_min_edit_distance_sum/2,
  choose_guess/1, edit_distance/4, count_edit_distances/2, get_min_tuple/1, build_edit_distance_tuples/3]).

minimum(X, Y, Z)  ->
  ValueList = [X, Y, Z],
  min(ValueList).

edit_distance(String1, String2, Len1, Len2) ->


  if
    Len1 == 0 ->
      Len2;
    Len2 == 0 ->
      Len1;
    true ->
      C1 = lists:sublist(String1, Len1, Len1),
      C2 = lists:sublist(String2, Len2, Len2),

      if
        C1 == C2 -> edit_distance(String1, String2, Len1 - 1, Len2 - 1);
        true -> 1 + minimum(edit_distance(String1, String2, Len1, Len2 - 1), edit_distance(String1, String2, Len1 - 1, Len2), edit_distance(String1, String2, Len1 - 1, Len2 - 1))
      end
  end.


list_to_variable([], OutList) -> OutList;
list_to_variable([E1 | EN], OutList) ->
  EN,
  Out = lists:append([E1 | EN], OutList),
  Out.

pick_min_edit_distance_sum([], Index) -> Index;
pick_min_edit_distance_sum([ED1 | EDN], Index)  ->
  if
    EDN < ED1 -> Index = Index+1;
    true -> pick_min_edit_distance_sum(EDN, Index)
  end.


count_edit_distances(G, []) ->
  G,
  0;
count_edit_distances(G, [Guess | NextGuess]) ->
  %%lists:append([edit_distance(G, Guess, length(G), length(Guess))], [count_edit_distances(G, NextGuess)]).
  edit_distance(G, Guess, length(G), length(Guess)) + count_edit_distances(G, NextGuess).


build_tuple_val_list(List) when length(List) == 0 -> [];
build_tuple_val_list(List) ->
  [element(2, hd(List)) | build_tuple_val_list(tl(List))].

get_min_val(List) ->
  lists:min(build_tuple_val_list(List)).

get_min_tuple(List) ->
  Val = get_min_val(List),
  if element(2, hd(List)) == Val -> hd(List);
    true -> get_min_tuple(tl(List))
  end.




build_edit_distance_tuples([], OutList, FullList) ->
  FullList,
  OutList;
build_edit_distance_tuples([E1 | EN], OutList, FullList) ->
  Out = lists:append(OutList, [{E1, count_edit_distances(E1, FullList)}]),
  build_edit_distance_tuples(EN, Out, FullList).
  %%



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





