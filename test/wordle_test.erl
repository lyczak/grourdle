-module(wordle_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

lowercase1_check_guess_test() ->
  Guess = "stand",
  Word  = "straw",
  Res = grdl_wordle:check_guess(Guess, Word),
  ?assertMatch({Guess, [green, green, yellow, grey, grey]}, {Guess, Res}).

duplicate_letter1_check_guess_test() ->
  Guess = "crass",
  Word  = "sales",
  Res = grdl_wordle:check_guess(Guess, Word),
  ?assertMatch({Guess, [grey, grey, yellow, yellow, green]}, {Guess, Res}).

all_green_check_guess_test() ->
  Guess = "bread",
  Word  = "bread",
  Res = grdl_wordle:check_guess(Guess, Word),
  ?assertMatch({Guess, [green, green, green, green, green]}, {Guess, Res}).

all_grey_check_guess_test() ->
  Guess = "ggggg",
  Word  = "bread",
  Res = grdl_wordle:check_guess(Guess, Word),
  ?assertMatch({Guess, [grey, grey, grey, grey, grey]}, {Guess, Res}).

all_yellow_check_guess_test() ->
  Guess = "bread",
  Word  = "rdaeb",
  Res = grdl_wordle:check_guess(Guess, Word),
  ?assertMatch({Guess, [yellow, yellow, yellow, yellow, yellow]}, {Guess, Res}).

-endif.