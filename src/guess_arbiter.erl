-module(guess_arbiter).

-import(lists,[min/1]).

-export([minimum/3, list_to_variable/2, pick_min_edit_distance_sum/2,
  choose_guess/1, edit_distance/4, count_edit_distances/2, get_min_tuple/1]).

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


%% @doc Pick/generate a Wordle guess from a list of several guesses.
choose_guess([Guess | NextGuess]) ->
  %%ChosenGuess = lists:nth(rand:uniform(length([Guess | NextGuess])), [Guess | NextGuess]),
  %%io:fwrite("~s",[ChosenGuess]).

  StringGuessesEmpty = [],
  StringGuesses = list_to_variable([Guess | NextGuess], StringGuessesEmpty),
  G1 = lists:nth(1, StringGuesses),
  G2 = lists:nth(2, StringGuesses),
  G3 = lists:nth(3, StringGuesses),
  SumED1 = count_edit_distances(G1, StringGuesses),
  SumED2 = count_edit_distances(G2, StringGuesses),
  SumED3 = count_edit_distances(G3, StringGuesses),
  Lst = [{G1, SumED1}, {G2, SumED2}, {G3, SumED3}], %% Take the minimum of this
  get_min_tuple(Lst).