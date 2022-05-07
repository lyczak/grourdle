-module(grdl_guess_arbiter).

-export([choose_guess/1, edit_distance/2, list_to_variable/2, count_edit_distances/2, get_min_tuple/1, build_tuple_val_list/1, make_min_tuple_list/2, get_min_val/1]).





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

%% @doc Calculates the editdistance from one string to another by calculating the number of character insertions, deletions, and moves it takes to change the first string to the second.
%% The edit_distance function itself calls compare on the the parameter strings to compare the character changes from the first string to the second string.
%% @param First, the first string used to measure the edit distance to a different string.
%% @param Second, the second string used for the end of the edit distance calculation.
%% @returns Returns a number (int) that is the total number of character insertions, deletions, and moves to change the first string to the second.
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
%% @doc Takes in a list that is recursively defined and assigns it to a variable.
%% Servers mostly an organizational purpose for creating a variable that contains a list of the current rounds guesses.
%% @param [], check for when the orginal list is done iterating through
%% @param OutList, the output variable that the input list is assigned to
%% @param [E1 | EN], the input list that is labeled as E1 for element 1 and EN for Elements...N
%% @returns Out, variable that the input list is assigned to
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

%% @doc Takes in one players guess, and sums the edit distances to each of the other guesses.
%% @param G, The Guess specified to have the edit distance to all the other guesses summed.
%% @param [], End of list check.
%% @param [Guess | NextGuess], Inputted list of each players guess.
%% @returns Number (int) of the sum of the edit distances from G to [Guess | NextGuess] (list of guesses).
count_edit_distances(G, []) ->
  G,
  0;
count_edit_distances(G, [Guess | NextGuess]) ->
  %%lists:append([edit_distance(G, Guess, length(G), length(Guess))], [count_edit_distances(G, NextGuess)]).
  edit_distance(G, Guess) + count_edit_distances(G, NextGuess).

%% @doc Builds a list of tuples containing each guess and the sum of edit distances to the other guesses.
%% @param List, List to be converted to a list of tuples.
%% @returns List of tuples containing edit disftances to other guesses.
build_tuple_val_list(List) when length(List) == 0 -> [];
build_tuple_val_list(List) ->
  [element(2, hd(List)) | build_tuple_val_list(tl(List))].

%% @doc simple function that returns the minimnum value in a list.
%% @param List, List that contains an min value.
%% @returns Number (int) that is the minimum value in the specified list.
get_min_val(List) ->
  lists:min(build_tuple_val_list(List)).

%% @doc Function that takes the minimum tuple and converts it to a list
%% @param List, List of Guesses
%% @param MinVal, minimum edit distance of Guesses.
%% @returns List containing the Chosen Guess and the correlated minimnum edit distance.
make_min_tuple_list(List, MinVal) ->
  if length(List) =:= 0 -> [];
    element(2, hd(List)) == MinVal -> [hd(List)] ++ make_min_tuple_list(tl(List), MinVal);
    true -> make_min_tuple_list(tl(List), MinVal)
  end.

%% @doc Function that returns the tuple that contains the minimum edit distance.
%% @param List, list of tuples that conatins one minium tuple value
%% @returns The tuple containing the minimum edit distance.
get_min_tuple(List) ->
  MinVal = get_min_val(List),
  TupleLst = make_min_tuple_list(List, MinVal),
  R0 = rand:uniform(length(TupleLst)),
  lists:nth(R0, TupleLst).

%% @doc Takes in the List of Guesses, and recursively iterates through each guess while calling helper functions to
%% build the edit distance tuples containing a Guess and its sum edit distance to the other Guesses.
%% @param [], Empty List Check
%% @param OutList, The List that is oputputed containiing a list of tuples with guesses and correspoinding edit distances.
%% @param FullList, Copy of the full list of guesses for the edit distance algorthim to reference for each guess while
%% iterating through the original List.
%% @returns List of Tuples containing each guess and its corresponding sum edit distance to the other guesses.
build_edit_distance_tuples([], OutList, FullList) ->
  FullList,
  OutList;
build_edit_distance_tuples([E1 | EN], OutList, FullList) ->
  Out = lists:append(OutList, [{E1, count_edit_distances(E1, FullList)}]),
  build_edit_distance_tuples(EN, Out, FullList).

%% @doc Pick/generate a Wordle guess from a list of several guesses based on minimum relative edit distance.
%% This function wraps the below one, for the case where the guesses are binaries.
choose_guess(Guesses = [First | _]) when is_binary(First) ->
  list_to_binary(
    choose_guess(
      lists:map(
        fun(G) -> binary_to_list(G) end,
        Guesses)));

%% @doc Pick/generate a Wordle guess from a list of several guesses based on minimum relative edit distance.
%% Main function that begins and ends guess_arbiter.
%% @param [Guess | NextGuess], List of inputted guesses for the round.
%% @returns The Chosen Guess of the round to be sent to the wordle game to be compared to the answer.
choose_guess([Guess | NextGuess]) ->

  EList1 = [],
  EList = [],
  FullList = list_to_variable([Guess | NextGuess], EList1),
  RecursiveLst = build_edit_distance_tuples([Guess | NextGuess], EList, FullList),
  ChosenGuess = lists:nth(1, tuple_to_list(get_min_tuple(RecursiveLst))),
  ChosenGuess.

  %%MinTuple = get_min_tuple(RecursiveLst), %%uncomment these two lines to be able to check the minimum tuple that contains the chosen guess and its sum edit distances from the other guesses
  %%MinTuple.





