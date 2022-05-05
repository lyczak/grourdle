-module(guess_arbiter_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_min_tuple_test()  ->
  CorrectAnswer = {"table", 4},
  List = [{"bread", 10}, {"table", 4}, {"movie", 16}],
  Check = grdl_guess_arbiter:get_min_tuple(List),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_basic_test() ->
  CorrectAnswer = 4,
  Check = grdl_guess_arbiter:edit_distance("table", "movie"),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_same_two_strings_test() ->
  CorrectAnswer = 0,
  Check = grdl_guess_arbiter:edit_distance("table", "table"),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_short_first_string_test() ->
  CorrectAnswer = 2,
  Check = grdl_guess_arbiter:edit_distance("tab", "table"),
  ?assertEqual(CorrectAnswer, Check).

edit_distance_short_second_string_test() ->
  CorrectAnswer = 2,
  Check = grdl_guess_arbiter:edit_distance("table", "tab"),
  ?assertEqual(CorrectAnswer, Check).

list_to_variable_test() ->
  CorrectAnswer = [1, 2, 3, 4],
  EmptyList = [],
  Check = grdl_guess_arbiter:list_to_variable(CorrectAnswer, EmptyList),
  ?assertEqual(CorrectAnswer, Check).

count_edit_distances_test() ->
  ListStrings = ["bread", "table", "mouse", "movie"],
  Check = grdl_guess_arbiter:count_edit_distances(lists:nth(3, ListStrings), ListStrings),
  CorrectAnswer = 11,
  ?assertEqual(CorrectAnswer, Check).

choose_guess_test() ->
  Check = grdl_guess_arbiter:choose_guess(["bread", "table", "movie", "hands"]),
  CorrectAnswer = "table",
  ?assertEqual(CorrectAnswer, Check).

-endif.