-module(guess_arbiter_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

duplicate_guess_check_guess_test() ->
  Guesses = ["learn", "stand", "soups", "stand"],
  Guess = guess_arbiter:choose_guess(Guesses),
  ?assertEqual("stand", Guess).

-endif.