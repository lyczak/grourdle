-module(guess_arbiter_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

minimum_test() ->
  Check = guess_arbiter:minimum(10, 4, 342),
  CorrectAnswer = 4,
  ?assertEqual(CorrectAnswer, Check).

edit_distance_basic_test() ->
  CorrectAnswer = 4,
  Check = guess_arbiter:edit_distance("table", "movie", 5, 5),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_same_two_strings_test() ->
  CorrectAnswer = 0,
  Check = guess_arbiter:edit_distance("table", "table", 5, 5),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_first_string_shorter_test() ->
  CorrectAnswer = 3,
  Check = guess_arbiter:edit_distance("tab", "table", 3, 5),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_second_string_shorter_test() ->
  CorrectAnswer = 3,
  Check = guess_arbiter:edit_distance("table", "tab", 5, 3),
  ?assertEqual(CorrectAnswer, Check).

list_to_variable_test() ->
  CorrectAnswer = [1, 2, 3, 4],
  EmptyList = [],
  Check = guess_arbiter:list_to_variable(CorrectAnswer, EmptyList),
  ?assertEqual(CorrectAnswer, Check).

count_edit_distances_test() ->
  ListStrings = ["bread", "table", "mouse", "movie"],
  Check = guess_arbiter:count_edit_distances(lists:nth(3, ListStrings), ListStrings),
  CorrectAnswer = 12,
  ?assertEqual(CorrectAnswer, Check).

choose_guess_test() ->
  Check = guess_arbiter:choose_guess(["bread", "table", "movie", "mouse"]),
  CorrectAnswer = "movie",
  ?assertEqual(CorrectAnswer, Check).


-endif.