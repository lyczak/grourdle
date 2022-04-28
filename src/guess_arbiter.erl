-module(guess_arbiter).

-export([choose_guess/1]).



%% @doc Pick/generate a Wordle guess from a list of several guesses.
choose_guess([First | Last]) ->
  Out = lists:nth(rand:uniform(length([First | Last])), [First | Last]),
  io:fwrite("~s",[Out]).


